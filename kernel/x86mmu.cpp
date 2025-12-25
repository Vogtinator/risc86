#include <stdio.h>
#include <string.h>

#include "x86mmu.h"
#include "utils.h"

void X86MMU::init()
{
	// Dynamically managed pages + two PML4 pages
	physPagesStart = physMemMgr.allocate((PHYS_PAGES + 2) * PAGE_SIZE, MemRegionPageTables);
	if (physPagesStart & (PAGE_SIZE - 1))
		panic("Allocation not page aligned");

	pml4p[0] = physPagesStart + (PHYS_PAGES * PAGE_SIZE);
	pml4p[1] = physPagesStart + ((PHYS_PAGES + 1) * PAGE_SIZE);

	// Mark all pages as free
	for (auto &e : freePageBitmap)
		e = ~0ul;

	// Populate PML4 from the bootloader mappings.
	// Only the first and last entries are used for guest VMA.
	PhysAddr efiloaderPML4phys;
	__asm volatile("mov %%cr3, %[pml4]\n" : [pml4] "=r" (efiloaderPML4phys));
	uint64_t *efiloaderPML4 = phys_to_virt<uint64_t>(efiloaderPML4phys);

	for (unsigned int i = 0; i < sizeof(pml4p) / sizeof(pml4p[0]); ++i) {
		uint64_t *pml4pVirt = phys_to_virt<uint64_t>(pml4p[i]);
		memcpy(pml4pVirt, efiloaderPML4, PAGE_SIZE);
	}
}

// There is a lot of optimization potential here:
// To avoid needless flushes of the whole context:
// * Implement ASIDs, keep non-active mappings cached
// * Free page tables on mapping removal
// * Keep track of global mappings and keep them on context reset
void X86MMU::resetContext()
{
	pml4pIdx ^= 1;
	PhysAddr pml4pNew = pml4p[pml4pIdx];
	uint64_t *pml4pNewVirt = phys_to_virt<uint64_t>(pml4pNew);
	pml4pNewVirt[0] = 0;
	pml4pNewVirt[511] = 0;

	__asm volatile("mov %[pml4], %%cr3\n" :: [pml4] "r" (pml4pNew));

	// Mark all pages as free
	for (auto &e : freePageBitmap)
		e = ~0ul;
}

void X86MMU::addRVMapping(uint64_t virtAddr, TranslationResult *rvMap)
{
	uint64_t flags = PT_PRESENT;
	if (rvMap->canWrite)
		flags |= PT_WRITABLE;
	if (rvMap->canUser)
		flags |= PT_USER;

	uint64_t phys = rvMap->phys_page_addr,
	         virt = virtAddr & ~rvMap->pageoff_mask,
	         size = rvMap->pageoff_mask + 1;

	while (size > 0) {
		auto mappedSize = doOneMapping(phys, virt, size, flags);
		phys += mappedSize;
		virt += mappedSize;
		size -= mappedSize;
		if (mappedSize == 0) {
			// On allocation failure, flush everything and try again.
			resetContext();
			return addRVMapping(virtAddr, rvMap);
		}
	}
}

void X86MMU::flushRVMapping(uintptr_t addr, size_t size)
{
	while (size > 0) {
		// Pass UINT64_MAX as size to flush whatever mapping is at addr,
		// also if size is 4k and the mapping at addr is a 1GiB huge page.
		auto flushedSize = doOneMapping(0, addr, UINT64_MAX & ~0xFFFul, PT_INVALID);
		asm volatile("invlpg (%[addr])" :: [addr] "r" (addr));
		if (flushedSize >= size)
			break;

		size -= flushedSize;
		addr += flushedSize;
	}
}

void X86MMU::flushRVMappingAtomic(uintptr_t addr, size_t size)
{
	asm volatile("cli");
	flushRVMapping(addr, size);
	asm volatile("sti");
}

// Separate function to ensure the redzone isn't used
 __attribute__((noinline))
static void switchToRing0() {
	// Syscall to return to ring 0
	asm volatile("int %[irq]" :: [irq] "i" (uint8_t(X86_IRQ_RING_0)));
}

void X86MMU::switchPrivileges(Priv priv)
{
	if (priv == Priv::Supervisor) {
		switchToRing0();
		return;
	}

	// Use iretq to "return" to ring 3.
	// Only CS and SS need to be switched, everything else is already DPL=3.
	asm volatile("mov %%rsp, %%rax\n"
	             "push %[ds]\n"
	             "push %%rax\n"
	             "pushfq\n"
	             "orq $(3 << 12), 0(%%rsp)\n" // Set IOPL in EFLAGS so that port IO keeps working
	             "push %[cs]\n"
	             "leaq 1f(%%rip), %%rax\n"
	             "pushq %%rax\n"
	             "iretq\n"
	             "1:\n"
	             :: [cs] "i" (SegmentUserCS), [ds] "i" (SegmentUserDS)
	             : "flags", "memory", "rax");
}

bool X86MMU::allocPhysPage(PhysAddr *addr)
{
	for (unsigned int i = 0; i < sizeof(freePageBitmap) / sizeof(freePageBitmap[0]); ++i) {
		auto bitOffset = __builtin_ffsl(freePageBitmap[i]);
		if (bitOffset == 0)
			continue;

		bitOffset -= 1;
		auto pageIndex = i * (sizeof(freePageBitmap[0]) * 8) + bitOffset;
		*addr = physPagesStart + (pageIndex * PAGE_SIZE);
		freePageBitmap[i] &= ~(1ul << bitOffset);

		// Clear allocated page
		auto *pageVirt = phys_to_virt<uint64_t>(*addr);
		for (unsigned int i = 0; i < PAGE_SIZE / sizeof(*pageVirt); ++i)
			pageVirt[i] = 0;

		return true;
	}

	return false;
}

void X86MMU::freePhysPage(PhysAddr addr)
{
	if (addr & (PAGE_SIZE - 1)
	    || addr < physPagesStart || addr > physPagesStart + (PHYS_PAGES * PAGE_SIZE))
		panic("Invalid pointer passed to freePhysPage");

	auto pageIndex = (addr - physPagesStart) / PAGE_SIZE;
	auto bitmapIndex = pageIndex / (sizeof(freePageBitmap[0]) * 8);
	auto bitOffset = pageIndex % (sizeof(freePageBitmap[0]) * 8);
	freePageBitmap[bitmapIndex] |= (1ul << bitOffset);
}

// Some optimization potential here: When converting a huge page to a page table,
// pre-fill it with the previous mapping?
size_t X86MMU::doOneMapping(uintptr_t phys, uintptr_t virt, uintptr_t size, uint64_t flags)
{
	if ((phys & 0xFFF) || (virt & 0xFFF) || (size & 0xFFF) || size == 0)
		panic("Called with invalid parameters");

	uint64_t *pml4 = phys_to_virt<uint64_t>(pml4p[pml4pIdx]);
	uint64_t *pml4e = &pml4[(virt >> 39) & 0x1FF];

	uint64_t *pdpt;
	if (*pml4e & PT_PRESENT) {
		if (*pml4e & PT_HUGEPAGE)
			panic("Unexpected level 4 huge page");
	} else if (!(flags & PT_PRESENT)) {
		// Don't bother to make a page directory just for non-present entries.
		return 1ul << 39;
	} else {
		PhysAddr page;
		if (!allocPhysPage(&page))
			return 0;

		*pml4e = page | PT_PRESENT | PT_WRITABLE | PT_USER;
	}

	pdpt = phys_to_virt<uint64_t>(*pml4e & 0x0003FFFFFFFFF000UL);

	uint64_t *pdpte = &pdpt[(virt >> 30) & 0x1FF];

	// 1GiB page?
	if (!(phys & 0x3FFFFFFF) && !(virt & 0x3FFFFFFF) && size >= 1 << 30) {
		*pdpte = phys | PT_HUGEPAGE | flags;
		return 1 << 30;
	}

	uint64_t *pd;
	if ((*pdpte & PT_PRESENT) && !(*pdpte & PT_HUGEPAGE)) {
		// Follow the existing table
	} else if (!(*pdpte & PT_PRESENT) && !(flags & PT_PRESENT)) {
		// Don't bother to make a page directory just for non-present entries.
		return 1 << 30;
	} else {
		PhysAddr page;
		if (!allocPhysPage(&page))
			return 0;

		*pdpte = page | PT_PRESENT | PT_WRITABLE | PT_USER;
	}

	pd = phys_to_virt<uint64_t>(*pdpte & 0x0003FFFFFFFFF000UL);

	uint64_t *pde = &pd[(virt >> 21) & 0x1FF];

	// 2MiB page?
	if (!(phys & 0x1FFFFF) && !(virt & 0x1FFFFF) && size >= 1 << 21) {
		*pde = phys | PT_HUGEPAGE | flags;
		return 1 << 21;
	}

	uint64_t *pt;
	if ((*pde & PT_PRESENT) && !(*pde & PT_HUGEPAGE)) {
		// Follow the existing table
	} else if (!(*pde & PT_PRESENT) && !(flags & PT_PRESENT)) {
		// Don't bother to make a page directory just for non-present entries.
		return 1 << 21;
	} else {
		PhysAddr page;
		if (!allocPhysPage(&page))
			return 0;

		*pde = page | PT_PRESENT | PT_WRITABLE | PT_USER;
	}

	pt = phys_to_virt<uint64_t>(*pde & 0x0003FFFFFFFFF000UL);

	uint64_t *pte = &pt[(virt >> 12) & 0x1FF];
	if (*pte && (flags & PT_PRESENT)) {
		// Already mapped.
		// This should not happen: If the mapping changes between accesses,
		// an sfence.vma should happen. Maybe this isn't done when the new
		// mapping becomes more permissible?
		// panic("Trying to map already existing page?");
	}

	*pte = phys | flags;
	return 1 << 12;
}
