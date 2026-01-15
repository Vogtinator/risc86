#pragma once

#include <stdint.h>

#include "mem.h"
#include "rvmmu.h"
#include "x86interrupts.h"

class X86MMU {
public:
	// Install IRQ handlers.
	static void initGlobal();
	// Allocate structures and initialize current mapping.
	void init();
	// Configure MMU on the current CPU.
	void initPerCPU();
	// Switch to a speciic ASID
	CALLED_FROM_IRQ void switchToContext(unsigned int asid);
	// Remove all (non-global) guest mappings for this ASID.
	CALLED_FROM_IRQ void resetContext(unsigned int asid);
	// Remove all (non-global) guest mappings for all ASIDs.
	CALLED_FROM_IRQ void resetAllContexts();
	// Add the RISC-V MMU mapping to the currently active page tables
	CALLED_FROM_IRQ void addRVMapping(uint64_t virtAddr, TranslationResult *rvMap);
	// Drop mappings from the active page tables
	CALLED_FROM_IRQ void flushRVMapping(uintptr_t addr, size_t size);
	// Same as above, but disables interrupts. Necessary to make sure
	// the tables are not messed with from and interrupt context
	// (e.g. through RFENCE). (A mutex would just deadlock)
	void flushRVMappingAtomic(uintptr_t addr, size_t size);

	enum class Priv { User, Supervisor };
	void switchPrivileges(Priv priv);

	static const unsigned int ASID_LOG2 = 3;
	static const unsigned int NUM_ASIDS = 1 << ASID_LOG2;
	static const unsigned int ASID_MASK = NUM_ASIDS - 1;
private:
	static const unsigned int PHYS_PAGES = 512;

	// Bitmap allocator for physical pages
	PhysAddr physPagesStart;
	using BitmapType = uint64_t;
	BitmapType freePageBitmap[PHYS_PAGES / (sizeof(BitmapType) * 8)];
	static_assert(PHYS_PAGES % (sizeof(BitmapType) * 8) == 0);

	// Returns false on failure
	CALLED_FROM_IRQ bool allocPhysPage(PhysAddr *addr);
	void freePhysPage(PhysAddr addr);

	typedef enum {
		PT_INVALID    = 0,
		PT_PRESENT    = 1 << 0,
		PT_WRITABLE   = 1 << 1,
		PT_USER       = 1 << 2,
		PT_HUGEPAGE   = 1 << 7,
		PT_NOEXEC     = 1ULL << 63,
	} PageTableFlags;

	CALLED_FROM_IRQ size_t doOneMapping(uintptr_t phys, uintptr_t virt, uintptr_t size, uint64_t flags);

	// PML4 tables for each RV ASID.
	PhysAddr pml4pForASID[NUM_ASIDS];
	unsigned int currentASID;
};
