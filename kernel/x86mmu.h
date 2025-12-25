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
	// Remove all (non-global) guest mappings and switch to the new set.
	CALLED_FROM_IRQ void resetContext();
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
private:
	static const unsigned int PHYS_PAGES = 64;

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

	size_t doOneMapping(uintptr_t phys, uintptr_t virt, uintptr_t size, uint64_t flags);

	// PML4 tables. Switching between them to flush non-global TLB entries.
	PhysAddr pml4p[2];
	bool pml4pIdx;
};
