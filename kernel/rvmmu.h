#pragma once

#include "hart.h"
#include "mem.h"
#include "x86interrupts.h"

// Result of an MMU translation.
// On failure, pageoff_mask is 0.
struct TranslationResult {
	uint64_t pageoff_mask;
	PhysAddr phys_page_addr;
	bool canRead, canWrite, canExec, canUser;
};

enum class AccessType {
	Read, Write, Exec
};

CALLED_FROM_IRQ TranslationResult mmu_translate(Hart *hart, uint64_t addr, AccessType type);
