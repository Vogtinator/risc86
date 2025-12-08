#include "rvmmu.h"
#include "utils.h"

#define PTE_V (1ull << 0)
#define PTE_R (1ull << 1)
#define PTE_W (1ull << 2)
#define PTE_X (1ull << 3)
#define PTE_U (1ull << 4)
#define PTE_G (1ull << 5)
#define PTE_A (1ull << 6)
#define PTE_D (1ull << 7)
#define PTE_PPN_SHIFT (10)
#define PTE_PPN_MASK ((1ull << 44) - 1)

TranslationResult mmu_translate(Hart *hart, uint64_t addr, AccessType type)
{
	uint32_t mode = hart->satp >> 60;

	if (mode == 0) {
		// Identity map: Return a 1GiB region that contains the address.
		uint64_t mask_1gb = (1UL << 30) - 1;
		TranslationResult ret = {
			.pageoff_mask = mask_1gb,
			.phys_page_addr = (addr & ~mask_1gb),
		};
		return ret;
	}

	if (mode != 8)
		panic("Not implemented %016lx", hart->satp);

	// Sv39 translation follows
	uint64_t sign = int64_t(addr) >> 39;
	if (sign != 0 && ~sign != 0)
		panic("Non-canonical address %016lx", addr);

	// Set stval and return an invalid translation
	auto fault = [&] () {
		return TranslationResult { 0, 0 };
	};

	PhysAddr pgtblPhys = (hart->satp & ((1ull << 44) - 1)) << 12;

	for (int lvl = 2; lvl >= 0; --lvl)
	{
		uint64_t *pgtbl = phys_to_virt<uint64_t>(pgtblPhys);
		uint32_t idx = (addr >> (12 + 9 * lvl)) & 0x1FF;
		uint64_t pte = pgtbl[idx];

		if (!(pte & PTE_V))
			return fault(); // Invalid

		if (!(pte & PTE_R) && (pte & PTE_W))
			return fault(); // Reserved encoding

		if (!(pte & (PTE_R | PTE_W | PTE_X)))
		{
			// Next level of page table
			pgtblPhys = ((pte >> PTE_PPN_SHIFT) & PTE_PPN_MASK) << 12;
			continue;
		}

		// TODO: SUM bit?
		if (hart->mode == Hart::MODE_USER && !(pte & PTE_U))
			return fault(); // User mode access denied

		// Check based on access type and AD bits
		// for software handling of those (Svade extension)
		bool allowed = false;
		switch (type)
		{
		case AccessType::Read:
			allowed = (pte & PTE_R) && (pte & PTE_A);
			break;
		case AccessType::Write:
			allowed = (pte & PTE_W) && (pte & PTE_A) && (pte & PTE_D);
			break;
		case AccessType::Exec:
			allowed = (pte & PTE_X) && (pte & PTE_A);
			break;
		}

		if (!allowed)
			return fault();

		uint64_t pageoff_mask = (1ull << (12 + 9 * lvl)) - 1;
		uint64_t ppn = ((pte >> PTE_PPN_SHIFT) & PTE_PPN_MASK) << 12;

		// Check alignment
		if (ppn & pageoff_mask)
			panic("PTE unaligned");

		return TranslationResult {
			.pageoff_mask = pageoff_mask,
			.phys_page_addr = PhysAddr(ppn),
		};
	}

	panic("Non-leaf PTE for vpn[0]");
	return fault();
}
