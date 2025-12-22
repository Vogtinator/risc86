#pragma once

#include <stddef.h>
#include <stdint.h>

#include "loaderapi.h"

using PhysAddr = uintptr_t;
using VirtAddr = void*;
static const size_t PAGE_SIZE = 4096;

template <typename T>
static T *phys_to_virt(uint64_t addr)
{
	return reinterpret_cast<T*>(KERNEL_PHYS_START + addr);
}

/* Class to manage physical memory in non-overlapping regions. */
class PhysMemMgr {
public:
	struct Region {
		PhysAddr start, end;
		MemoryRegionType type;
	};
	
	// Add a region of the specified type. Must not be overlapping with any existing region.
	void addRegion(PhysAddr start, PhysAddr end, MemoryRegionType type);

	// Allocate contiguous physical memory with given size. No alignment guaranteees.
	PhysAddr allocate(size_t size, MemoryRegionType type);

	// Print regions to the console for debugging.
	void print();

	// Get the total size of MemRegionFree regions.
	size_t totalFreeBytes();

	// Marks remaining free regions as MemRegionPayload
	void markRestForPayload();

	// Iterate all MemRegionPayload regions
	template <typename T>
	void iteratePayloadRegions(const T &callback)
	{
		for(size_t i = 0; i < regionCount; ++i)
			if (regions[i].type == MemRegionPayload)
				callback(regions[i]);
	}

private:
	size_t regionCount = 0;
	Region regions[128];
};

extern PhysMemMgr physMemMgr;
extern KernelParams kernel_params;
