#include "rvmmu.h"
#include "utils.h"

TranslationResult mmu_translate(HartState *hart, uint64_t addr, AccessType type)
{
	if(hart->satp != 0)
		panic("Not implemented");

	// Identity map: Return a 1GiB region that contains the address.
	uint64_t mask_1gb = (1UL << 30) - 1;
	TranslationResult ret = {
		.pageoff_mask = mask_1gb,
		.phys_page_addr = (addr & ~mask_1gb),
	};
	return ret;
}
