#include <efi.h>

#define PAGE_SIZE 4096

// Internal kernel ABI constants
static const UINTN KERNEL_STACK_LOW  = 0xFFFF800000000000UL,
                   KERNEL_STACK_SIZE = 12 * 1024 * 1024,     // 12MiB
                   KERNEL_LOAD_ADDR  = 0xFFFF810000000000UL,
                   KERNEL_PHYS_START = 0xFFFF900000000000UL,
                   KERNEL_PHYS_END   = 0xFFFFA00000000000UL;

// Embedded raw kernel binary to load
alignas(PAGE_SIZE) static const UINT8 kernel[] = {
#embed KERNEL_BIN_PATH
};

struct KernelParams {
	uint64_t kernel_phys, kernel_len;
	uint64_t initrd_phys, initrd_len;
	struct MemoryAttrib {
		uint64_t start, size;
		uint64_t flags;
	} memory_regions[256];
} __attribute__ ((packed));

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

static void *allocPages(UINTN count)
{
	EFI_PHYSICAL_ADDRESS ptr;
	// TODO: Correct type? Can't be freed.
	if (ST->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, count, &ptr) != EFI_SUCCESS)
		return NULL;

	return (void*) ptr;
}

static UINTN *allocPageTable()
{
	UINTN *ret = allocPages(1);
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

EFI_STATUS efi_main(EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable)
{
    EFI_STATUS Status;

	ST = SystemTable;

	// Sign of life
	Status = SystemTable->ConOut->OutputString(SystemTable->ConOut, L"Loading kernel...\r\n");
	if (EFI_ERROR(Status))
		return Status;

	// Load kernel and initrd
	// TODO

	// Allocate pml4
	pml4 = allocPageTable();
	if (!pml4)
		return EFI_OUT_OF_RESOURCES;

	// Allocate stack
	EFI_PHYSICAL_ADDRESS stackPhys = (EFI_PHYSICAL_ADDRESS) allocPages(KERNEL_STACK_SIZE / PAGE_SIZE);
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
	// TODO: Correct type? Can be freed.
	Status = ST->BootServices->AllocatePool(EfiLoaderData, MemoryMapAllocSize, (void**) &MemoryMap);
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

	struct KernelParams *params = NULL;
	// TODO: Correct type? Can't be freed.
	Status = ST->BootServices->AllocatePool(EfiLoaderData, sizeof(struct KernelParams), (void**) &params);
	if (EFI_ERROR(Status))
		return Status;

	/* TODO: params->initrd_phys = ... */

	// Get the final memory map
	MemoryMapSize = MemoryMapAllocSize;
	Status = ST->BootServices->GetMemoryMap(&MemoryMapSize, MemoryMap, &MapKey, &DescriptorSize, &DescriptorVersion);
	if (EFI_ERROR(Status))
		return Status;

	entry_count = MemoryMapSize / DescriptorSize;
	if (entry_count >= sizeof(params->memory_regions) / sizeof(params->memory_regions[0]))
		return EFI_OUT_OF_RESOURCES;

	for(UINTN i = 0; i < entry_count; ++i) {
		EFI_MEMORY_DESCRIPTOR *map_entry = (EFI_MEMORY_DESCRIPTOR*) ((UINTN)MemoryMap + i * DescriptorSize);
		params->memory_regions[i].start = map_entry->PhysicalStart;
		params->memory_regions[i].size = map_entry->NumberOfPages * PAGE_SIZE;
		params->memory_regions[i].flags = 0xcafebeef; // TODO
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
	               "D" (params), // First parameter to the kernel
	               [stack_high] "r" (KERNEL_STACK_LOW + KERNEL_STACK_SIZE - 8), // -8 for alignment as if it was a call
				   [kernel] "r" (KERNEL_LOAD_ADDR));

	__builtin_unreachable();
}
