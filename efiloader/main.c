#include <efi.h>

#define PAGE_SIZE 4096

// Internal kernel ABI constants
static const UINTN KERNEL_STACK_LOW  = 0xFFFF800000000000UL,
                   KERNEL_STACK_SIZE = 12 * 1024 * 1024,     // 12MiB
                   KERNEL_LOAD_ADDR  = 0xFFFF810000000000UL;

// Embedded raw kernel binary to load
alignas(PAGE_SIZE) static const UINT8 kernel[] = {
#embed "../kernel/kernel.bin"
};

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

static UINTN *allocPageTable()
{
	EFI_PHYSICAL_ADDRESS ptr;
	// TODO: Correct type?
	if (ST->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, 1, &ptr) != EFI_SUCCESS)
		return NULL;

	UINTN *ret = (UINTN*) ptr;
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

	// Greeting.
	Status = SystemTable->ConOut->OutputString(SystemTable->ConOut, L"Loading kernel...\r\n");
	if (EFI_ERROR(Status))
		return Status;

	// Allocate pml4
	pml4 = allocPageTable();
	if (!pml4)
		return EFI_OUT_OF_RESOURCES;

	// Allocate stack
	EFI_PHYSICAL_ADDRESS stackPhys;
	// TODO: Correct type?
	if (ST->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, KERNEL_STACK_SIZE / PAGE_SIZE, &stackPhys) != EFI_SUCCESS)
		return EFI_OUT_OF_RESOURCES;

	// Map stack
	Status = mmap(stackPhys, KERNEL_STACK_LOW, KERNEL_STACK_SIZE, PT_NOEXEC | PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Map kernel
	Status = mmap((EFI_PHYSICAL_ADDRESS) &kernel, KERNEL_LOAD_ADDR, (sizeof(kernel) + 0xFFF) & ~0xFFFULL, PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Get memory map and figure out range for identity map

	// Perform identity map so that paging can be enabled
	Status = mmap(0, 0, 1 << 30, PT_WRITABLE | PT_PRESENT);
	if (EFI_ERROR(Status))
		return Status;

	// Exit boot services
	/*Status = SystemTable->BootServices->ExitBootServices(ImageHandle, 0); // TODO: MapKey?
	if (EFI_ERROR(Status))
		return Status;*/

	// Enable paging
	__asm volatile("mov %[pml4], %%cr3\n" :: [pml4] "r" (pml4));

	// Set up stack and jump to kernel
	__asm volatile("mov %[stack_high], %%rsp\n"
	               "jmp *%[kernel]" ::
	               [stack_high] "r" (KERNEL_STACK_LOW + KERNEL_STACK_SIZE),
				   [kernel] "r" (KERNEL_LOAD_ADDR));

	__builtin_unreachable();
}
