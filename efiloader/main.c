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

static UINTN *alloc_page_table()
{
	EFI_PHYSICAL_ADDRESS ptr;
	// TODO: Correct type?
	if (ST->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, 1, &ptr) != EFI_SUCCESS)
		return NULL;

	UINTN *ret = (UINTN*) ptr;
	memset(ret, 0, PAGE_SIZE);
	return ret;
}

static UINTN *pml4 = NULL;

// TODO: Flags (RWX, U/S)
static EFI_STATUS mmapOne(UINTN phys, UINTN virt, UINTN size)
{
	virt = virt & 0x0000FFFFFFFFFFFF;

	if ((phys & 0xFFF) || (virt & 0xFFF) || (size & 0xFFF) || size == 0)
		return EFI_INVALID_PARAMETER;

	UINTN *pml4e = &pml4[(virt >> 39) & 0x1FF];

	UINTN *pdpt;
	if (*pml4e) {
		pdpt = (UINTN*)(*pml4e & 0x7FFFFFFFFFFFF000UL); // TODO: Mask out more reserved bits
	} else {
		pdpt = alloc_page_table();
		if (!pdpt)
			return EFI_OUT_OF_RESOURCES;

		*pml4e = (EFI_PHYSICAL_ADDRESS) pdpt | 0b11;
	}

	UINTN *pdpte = &pdpt[(virt >> 30) & 0x1FF];

	// 1GiB page?
	if (!(phys & 0x3FFFFFFF) && !(virt & 0x3FFFFFFF) && !(size & 0x3FFFFFFF)) {
		*pdpte = (EFI_PHYSICAL_ADDRESS) phys | 0b10000011;
		return 1 << 30;
	}

	UINTN *pd;
	if (*pdpte) {
		pd = (UINTN*)(*pdpte & 0x7FFFFFFFFFFFF000UL); // TODO: Mask out more reserved bits
	} else {
		pd = alloc_page_table();
		if (!pd)
			return EFI_OUT_OF_RESOURCES;

		*pdpte = (EFI_PHYSICAL_ADDRESS) pd | 0b11;
	}

	UINTN *pde = &pd[(virt >> 21) & 0x1FF];

	// 2MiB page?
	if (!(phys & 0x1FFFFF) && !(virt & 0x1FFFFF) && !(size & 0x1FFFFF)) {
		*pde = (EFI_PHYSICAL_ADDRESS) phys | 0b10000011;
		return 1 << 21;
	}

	UINTN *pt;
	if (*pde) {
		pt = (UINTN*)(*pde & 0x7FFFFFFFFFFFF000UL); // TODO: Mask out more reserved bits
	} else {
		pt = alloc_page_table();
		if (!pt)
			return EFI_OUT_OF_RESOURCES;

		*pde = (EFI_PHYSICAL_ADDRESS) pt | 0b11;
	}

	UINTN *pte = &pt[(virt >> 12) & 0x1FF];
	if (*pte) {
		// TODO: Already mapped
	}

	*pte = (EFI_PHYSICAL_ADDRESS) phys | 0b11;

	return 1 << 12;
}

static EFI_STATUS mmap(UINTN phys, UINTN virt, UINTN size)
{
	while (size > 0) {
		EFI_STATUS sizeMapped = mmapOne(phys, virt, size);
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
    EFI_INPUT_KEY Key;

	ST = SystemTable;

    /* Say hi */
	Status = SystemTable->ConOut->OutputString(SystemTable->ConOut, L"Hello World\r\n");
	if (EFI_ERROR(Status))
		return Status;

	// Allocate pml4
	pml4 = alloc_page_table();
	if (!pml4)
		return EFI_OUT_OF_RESOURCES;

	// Allocate stack

	// Map stack

	// Map kernel
	Status = mmap((EFI_PHYSICAL_ADDRESS) &kernel, KERNEL_LOAD_ADDR, (sizeof(kernel) + 0xFFF) & ~0xFFFULL);
	if (EFI_ERROR(Status))
		return Status;

	// Get memory map and figure out range for identity map

	// Perform identity map so that paging can be enabled
	Status = mmap(0, 0, 1 << 30);
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
	               [stack_high] "r" (KERNEL_STACK_LOW + KERNEL_STACK_SIZE), // TODO: Or -16 here?
				   [kernel] "r" (KERNEL_LOAD_ADDR));

    /* Now wait for a keystroke before continuing, otherwise your
       message will flash off the screen before you see it.

       First, we need to empty the console input buffer to flush
       out any keystrokes entered before this point */
    Status = SystemTable->ConIn->Reset(SystemTable->ConIn, FALSE);
    if (EFI_ERROR(Status))
        return Status;

    /* Now wait until a key becomes available.  This is a simple
       polling implementation.  You could try and use the WaitForKey
       event instead if you like */
    while ((Status = SystemTable->ConIn->ReadKeyStroke(SystemTable->ConIn, &Key)) == (EFI_STATUS)EFI_NOT_READY);

    return Status;
}
