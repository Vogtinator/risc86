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

TranslationResult mmu_translate(HartState *hart, uint64_t addr, AccessType type)
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

	auto permissionCheck = [&] (uint64_t pte) -> bool {
		if (!(pte & PTE_V))
			return false; // Invalid

		if (!(pte & PTE_R) && (pte & PTE_W))
			return false; // Reserved encoding

		if (!(pte & (PTE_R | PTE_W | PTE_X)))
			return true; // non-leaf PTE

		// TODO: SUM bit?
		if (hart->mode == HartState::MODE_USER && !(pte & PTE_U))
			return false;

		// Check based on access type and AD bits
		// for software handling of those (Svade extension)
		switch (type)
		{
		case AccessType::Read:
			return (pte & PTE_R) && (pte & PTE_A);
		case AccessType::Write:
			return (pte & PTE_W) && (pte & PTE_A) && (pte & PTE_D);
		case AccessType::Exec:
			return (pte & PTE_X) && (pte & PTE_A);
		}
	};

	auto fault = [&] () {
		hart->stval = addr;
		return TranslationResult { 0, 0 };
	};

	// VPN[2] lookup
	PhysAddr vpn2tblphys = (hart->satp & ((1ull << 44) - 1)) << 12;
	uint64_t *vpn2tbl = phys_to_virt<uint64_t>(vpn2tblphys);
	uint32_t vpn2 = (addr >> 30) & 0x1FF;
	uint64_t vpn2pte = vpn2tbl[vpn2];

	if (!permissionCheck(vpn2pte))
		return fault();

	if (vpn2pte & (PTE_R | PTE_W | PTE_X)) { // 1GB page
		panic("Not implemented");
	}

	panic("Addr %016lx Table at %lx Entry: %lx\n", addr, vpn2tblphys, vpn2pte);
}
