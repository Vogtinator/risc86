#include <efi.h>
#include "../kernel/loaderapi.h"

#define PAGE_SIZE 4096

// Custom EFI_MEMORY_TYPE
#define MEMORY_TYPE_PAYLOAD 0x80000001U

// Embedded raw kernel binary to load
alignas(PAGE_SIZE) static const UINT8 kernel[] = {
#embed KERNEL_BIN_PATH
};

static struct KernelParams params = { 0 };

void *memcpy(void *target, void *src, size_t len)
{
	uint8_t *target8 = target, *src8 = src;;
	while(len--) *target8++ = *src8++;

	return target;
}

void *memset(void *ptr, uint8_t val, size_t len)
{
	uint8_t *ptr8 = ptr;
	while(len--)
		*ptr8++ = val;

	return ptr;
}

static EFI_SYSTEM_TABLE *ST = NULL;

static void *allocPages(UINTN count, EFI_MEMORY_TYPE Type)
{
	EFI_PHYSICAL_ADDRESS ptr;
	if (ST->BootServices->AllocatePages(AllocateAnyPages, Type, count, &ptr) != EFI_SUCCESS)
		return NULL;

	return (void*) ptr;
}

static UINTN *allocPageTable()
{
	UINTN *ret = allocPages(1, EfiLoaderData);
	if (!ret)
		return NULL;

	memset(ret, 0, PAGE_SIZE);
	return ret;
}

typedef enum {
	PT_PRESENT    = 1 << 0,
	PT_WRITABLE   = 1 << 1,
	PT_SUPERVISOR = 1 << 2,
	PT_HUGEPAGE   = 1 << 7,
	PT_NOEXEC     = 1ULL << 63,
} PageTableFlags;

static UINTN *pml4 = NULL;

static EFI_STATUS mmapOne(UINTN phys, UINTN virt, UINTN size, PageTableFlags flags)
{
	if ((phys & 0xFFF) || (virt & 0xFFF) || (size & 0xFFF) || size == 0)
		return EFI_INVALID_PARAMETER;

	UINTN *pml4e = &pml4[(virt >> 39) & 0x1FF];

	UINTN *pdpt;
	if (*pml4e & PT_PRESENT) {
		if (*pml4e & PT_HUGEPAGE)
			return EFI_UNSUPPORTED;
		else
			pdpt = (UINTN*)(*pml4e & 0x0003FFFFFFFFF000UL);
	} else {
		pdpt = allocPageTable();
		if (!pdpt)
			return EFI_OUT_OF_RESOURCES;

		*pml4e = (EFI_PHYSICAL_ADDRESS) pdpt | PT_PRESENT | PT_WRITABLE;
	}

	UINTN *pdpte = &pdpt[(virt >> 30) & 0x1FF];

	// 1GiB page?
	if (!(phys & 0x3FFFFFFF) && !(virt & 0x3FFFFFFF) && size >= 1 << 30) {
		*pdpte = (EFI_PHYSICAL_ADDRESS) phys | PT_HUGEPAGE | flags;
		return 1 << 30;
	}

	UINTN *pd;
	if (*pdpte & PT_PRESENT) {
		if (*pdpte & PT_HUGEPAGE)
			return EFI_UNSUPPORTED;
		else
			pd = (UINTN*)(*pdpte & 0x0003FFFFFFFFF000UL);
	} else {
		pd = allocPageTable();
		if (!pd)
			return EFI_OUT_OF_RESOURCES;

		*pdpte = (EFI_PHYSICAL_ADDRESS) pd | PT_PRESENT | PT_WRITABLE;
	}

	UINTN *pde = &pd[(virt >> 21) & 0x1FF];

	// 2MiB page?
	if (!(phys & 0x1FFFFF) && !(virt & 0x1FFFFF) && size >= 1 << 21) {
		*pde = (EFI_PHYSICAL_ADDRESS) phys | PT_HUGEPAGE | flags;
		return 1 << 21;
	}

	UINTN *pt;
	if (*pde & PT_PRESENT) {
		if (*pde & PT_HUGEPAGE)
			return EFI_UNSUPPORTED;
		else
			pt = (UINTN*)(*pde & 0x0003FFFFFFFFF000UL);
	} else {
		pt = allocPageTable();
		if (!pt)
			return EFI_OUT_OF_RESOURCES;

		*pde = (EFI_PHYSICAL_ADDRESS) pt | PT_PRESENT | PT_WRITABLE;
	}

	UINTN *pte = &pt[(virt >> 12) & 0x1FF];
	if (*pte) {
		// Already mapped
		return EFI_INVALID_PARAMETER;
	}

	*pte = (EFI_PHYSICAL_ADDRESS) phys | flags;
	return 1 << 12;
}

static EFI_STATUS mmap(UINTN phys, UINTN virt, UINTN size, PageTableFlags flags)
{
	while (size > 0) {
		EFI_STATUS sizeMapped = mmapOne(phys, virt, size, flags);
		if (EFI_ERROR(sizeMapped)) {
			return sizeMapped;
		}

		phys += (UINTN) sizeMapped;
		virt += (UINTN) sizeMapped;
		size -= (UINTN) sizeMapped;
	}

	return EFI_SUCCESS;
}

static EFI_STATUS openVolumeOfImage(EFI_HANDLE ImageHandle, EFI_FILE_HANDLE *Volume)
{
	EFI_LOADED_IMAGE *loaded_image = NULL;
	EFI_GUID lipGuid = EFI_LOADED_IMAGE_PROTOCOL_GUID;
	EFI_GUID fsGuid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;

	EFI_STATUS Status = ST->BootServices->HandleProtocol(ImageHandle, &lipGuid, (void**)&loaded_image);
	if (EFI_ERROR(Status))
		return Status;

	EFI_FILE_IO_INTERFACE *IOVolume = NULL;
	Status = ST->BootServices->HandleProtocol(loaded_image->DeviceHandle, &fsGuid, (void*)&IOVolume);
	if (EFI_ERROR(Status))
		return Status;

	return IOVolume->OpenVolume(IOVolume, Volume);
}

static EFI_STATUS openFileInVolume(EFI_FILE_HANDLE Volume, CHAR16 *FileName, EFI_FILE_HANDLE *FileHandle, UINT64 *FileSize)
{
	EFI_STATUS Status = Volume->Open(Volume, FileHandle, FileName, EFI_FILE_MODE_READ, 0);
	if (EFI_ERROR(Status))
		return Status;

	// Get size of file by seeking to the end, ...
	Status = (*FileHandle)->SetPosition(*FileHandle, ~(UINT64)0);
	if (EFI_ERROR(Status))
		return Status;

	// ... querying the position, ...
	Status = (*FileHandle)->GetPosition(*FileHandle, FileSize);
	if (EFI_ERROR(Status))
		return Status;

	// ... and seeking back again.
	return (*FileHandle)->SetPosition(*FileHandle, 0);
}

EFI_STATUS loadKernelAndInitrd(EFI_HANDLE ImageHandle)
{
	// Open a handle to the ESP
	EFI_FILE_HANDLE Volume;
	EFI_STATUS Status = openVolumeOfImage(ImageHandle, &Volume);
	if (EFI_ERROR(Status))
		return Status;

	// Open handles to kernel and initrd
	EFI_FILE_HANDLE KernelFile, InitrdFile;
	UINT64 KernelSize, InitrdSize;
	Status = openFileInVolume(Volume, L"Image", &KernelFile, &KernelSize);
	if (EFI_ERROR(Status))
		return Status;

	Status = openFileInVolume(Volume, L"initrd", &InitrdFile, &InitrdSize);
	if (EFI_ERROR(Status))
		return Status;

	// Alloate memory region for initrd + kernel
	// Add worst case padding for 2MiB kernel alignment.
	UINTN payloadSize = InitrdSize + 2*1024*1024 + KernelSize;
	VOID *payloadArea = allocPages((payloadSize + PAGE_SIZE - 1) / PAGE_SIZE, MEMORY_TYPE_PAYLOAD);
	if (payloadArea == NULL)
		return EFI_OUT_OF_RESOURCES;

	params.initrd_phys = (uint64_t) payloadArea;
	params.initrd_len = InitrdSize;
	params.kernel_phys = (params.initrd_phys + params.initrd_len + 2*1024*1024 - 1) & ~(UINT64)(2*1024*1024 - 1);
	params.kernel_len = KernelSize;

	// Load kernel into memory
	UINT64 ActuallyRead = KernelSize;
	Status = KernelFile->Read(KernelFile, &ActuallyRead, (void*)(params.kernel_phys));
	if (EFI_ERROR(Status) || ActuallyRead != KernelSize)
		return Status;

	// Load initrd into memory
	ActuallyRead = InitrdSize;
	Status = InitrdFile->Read(InitrdFile, &ActuallyRead, (void*)(params.initrd_phys));
	if (EFI_ERROR(Status) || ActuallyRead != InitrdSize)
		return Status;

	// Close all handles again
	InitrdFile->Close(InitrdFile);
	KernelFile->Close(KernelFile);
	Volume->Close(Volume);

	return EFI_SUCCESS;
}

EFI_STATUS efi_main(EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable)
{
    EFI_STATUS Status;

	ST = SystemTable;

	// Sign of life
	Status = SystemTable->ConOut->OutputString(SystemTable->ConOut, L"Loading risc86...\r\n");
	if (EFI_ERROR(Status))
		return Status;

	// Load kernel and initrd
	loadKernelAndInitrd(ImageHandle);

	// Allocate pml4
	pml4 = allocPageTable();
	if (!pml4)
		return EFI_OUT_OF_RESOURCES;

	// Allocate stack
	EFI_PHYSICAL_ADDRESS stackPhys = (EFI_PHYSICAL_ADDRESS) allocPages(KERNEL_STACK_SIZE / PAGE_SIZE, EfiLoaderData);
	if (!stackPhys)
		return EFI_OUT_OF_RESOURCES;

	// Map stack
	Status = mmap(stackPhys, KERNEL_STACK_LOW, KERNEL_STACK_SIZE, PT_NOEXEC | PT_SUPERVISOR | PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Map kernel
	Status = mmap((EFI_PHYSICAL_ADDRESS) &kernel, KERNEL_LOAD_ADDR, (sizeof(kernel) + 0xFFF) & ~0xFFFULL, PT_SUPERVISOR | PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Get memory map and figure out range for identity map
	UINTN MemoryMapSize = 0, MapKey = 0, DescriptorSize = 0;
	UINT32 DescriptorVersion = 0;

	// Get size first
	Status = ST->BootServices->GetMemoryMap(&MemoryMapSize, NULL, &MapKey, &DescriptorSize, &DescriptorVersion);
	if (Status != EFI_BUFFER_TOO_SMALL)
		return Status;

	// Allocate space for the memory map + some slack
	UINTN MemoryMapAllocSize = MemoryMapSize + 16 * DescriptorSize;
	EFI_MEMORY_DESCRIPTOR *MemoryMap = NULL;
	// EfiBootServicesData -> can be reused by the kernel
	Status = ST->BootServices->AllocatePool(EfiBootServicesData, MemoryMapAllocSize, (void**) &MemoryMap);
	if (EFI_ERROR(Status))
		return Status;

	MemoryMapSize = MemoryMapAllocSize;
	Status = ST->BootServices->GetMemoryMap(&MemoryMapSize, MemoryMap, &MapKey, &DescriptorSize, &DescriptorVersion);
	if (EFI_ERROR(Status))
		return Status;

	// Get the end of physical memory
	UINTN entry_count = MemoryMapSize / DescriptorSize;
	UINTN phys_addr_max = 0;
	for(UINTN i = 0; i < entry_count; ++i) {
		EFI_MEMORY_DESCRIPTOR *map_entry = (EFI_MEMORY_DESCRIPTOR*) ((UINTN)MemoryMap + i * DescriptorSize);
		UINTN entry_phys_end = map_entry->PhysicalStart + map_entry->NumberOfPages * PAGE_SIZE;
		if (phys_addr_max < entry_phys_end)
			phys_addr_max = entry_phys_end;
	}

	if (KERNEL_PHYS_START + phys_addr_max >= KERNEL_PHYS_END)
		return EFI_INVALID_PARAMETER;

	// Perform identity map so that paging can be enabled
	Status = mmap(0, 0, phys_addr_max, PT_SUPERVISOR | PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Map physical memory into kernel space
	Status = mmap(0, KERNEL_PHYS_START, phys_addr_max, PT_SUPERVISOR | PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Get the final memory map
	MemoryMapSize = MemoryMapAllocSize;
	Status = ST->BootServices->GetMemoryMap(&MemoryMapSize, MemoryMap, &MapKey, &DescriptorSize, &DescriptorVersion);
	if (EFI_ERROR(Status))
		return Status;

	entry_count = MemoryMapSize / DescriptorSize;
	if (entry_count >= sizeof(params.memory_regions) / sizeof(params.memory_regions[0]))
		return EFI_OUT_OF_RESOURCES;

	for(UINTN i = 0; i < entry_count; ++i) {
		EFI_MEMORY_DESCRIPTOR *map_entry = (EFI_MEMORY_DESCRIPTOR*) ((UINTN)MemoryMap + i * DescriptorSize);

		enum MemoryRegionType type = ~0;
		switch (map_entry->Type)
		{
			case EfiBootServicesCode:
			case EfiBootServicesData:
			case EfiConventionalMemory:
				type = MemRegionFree;
				break;
			case MEMORY_TYPE_PAYLOAD:
				type = MemRegionPayload;
				break;
			default:
				continue;
		}

		// Merge with previous entry if possible
		if (params.memory_region_count > 0
			&& params.memory_regions[params.memory_region_count - 1].type == type
			&& (params.memory_regions[params.memory_region_count - 1].start + params.memory_regions[params.memory_region_count - 1].size) == map_entry->PhysicalStart)
		{
			params.memory_regions[params.memory_region_count - 1].size += map_entry->NumberOfPages * PAGE_SIZE;
			continue;
		}

		params.memory_regions[params.memory_region_count].start = map_entry->PhysicalStart;
		params.memory_regions[params.memory_region_count].size = map_entry->NumberOfPages * PAGE_SIZE;
		params.memory_regions[params.memory_region_count].type = type;
		params.memory_region_count++;
	}

	// Exit boot services
	Status = SystemTable->BootServices->ExitBootServices(ImageHandle, MapKey);
	if (EFI_ERROR(Status))
		return Status;

	// Enable paging
	__asm volatile("mov %[pml4], %%cr3\n" :: [pml4] "r" (pml4));

	// Set up stack and jump to kernel
	__asm volatile("mov %[stack_high], %%rsp\n"
	               "jmp *%[kernel]" ::
	               "D" (&params), // First parameter to the kernel
	               [stack_high] "r" (KERNEL_STACK_LOW + KERNEL_STACK_SIZE - 8), // -8 for alignment as if it was a call
				   [kernel] "r" (KERNEL_LOAD_ADDR));

	__builtin_unreachable();
}
