#pragma once

#include "hart.h"
#include "mem.h"

// Result of an MMU translation.
// On failure, pageoff_mask is 0.
struct TranslationResult {
	uint64_t pageoff_mask;
	PhysAddr phys_page_addr;
};

enum class AccessType {
	Read, Write, Exec
};

// On failure, sets hart->stval accordingly.
TranslationResult mmu_translate(HartState *hart, uint64_t addr, AccessType type);
