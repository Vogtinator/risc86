#include <string.h>
#include <stdio.h>

#include "mem.h"
#include "utils.h"

PhysMemMgr physMemMgr;

void PhysMemMgr::addRegion(PhysAddr start, PhysAddr end, MemoryRegionType type)
{
	if (regionCount >= sizeof(regions) / sizeof(*regions))
		panic("No space to add memory region");

	for(size_t i = 0; i <= regionCount; ++i) {
		if(i != 0 && start <= regions[i - 1].end)
			panic("Overlapping memory region");

		if(i != regionCount && end >= regions[i].start)
			continue;

		// efiloader already merges adjacent regions of the same type,
		// so no need to do this here.

		// Move succeeding elements to free space
		memmove(&regions[i + 1], &regions[i], (regionCount - i) * sizeof(*regions));
		regionCount++;
		regions[i] = {.start = start, .end = end, .type = type};
		return;
	}

	panic("Failed to add memory region"); // Unreachable
}

PhysAddr PhysMemMgr::allocate(size_t size, MemoryRegionType type)
{
	if (regionCount >= sizeof(regions) / sizeof(*regions))
		panic("No space to add memory region for allocation (size %lu, type %u)", size, type);

	for(size_t i = 0; i < regionCount; ++i) {
		// Find a free region with enough space
		if (regions[i].type != MemRegionFree
		    || regions[i].end - regions[i].start + 1 < size)
			continue;

		// Create a new region with the requested size and type
		memmove(&regions[i + 1], &regions[i], (regionCount - i) * sizeof(*regions));
		regionCount++;
		regions[i] = {.start = regions[i + 1].start, .end = regions[i + 1].start + size - 1, .type = type};
		regions[i + 1].start += size;
		return regions[i].start;
	}

	panic("No space to allocate %lu bytes of type %u", size, type);
}

void PhysMemMgr::print()
{
	for(size_t i = 0; i < regionCount; ++i)
		printf("Region %02ld %09lx-%09lx %08d\n", i, regions[i].start, regions[i].end, regions[i].type);
}

size_t PhysMemMgr::totalFreeBytes()
{
	size_t ret = 0;
	for(size_t i = 0; i < regionCount; ++i)
		if (regions[i].type == MemRegionFree)
			ret += regions[i].end - regions[i].start + 1;

	return ret;
}

void PhysMemMgr::markRestForPayload()
{
	for(size_t i = 0; i < regionCount; ++i)
		if (regions[i].type == MemRegionFree)
			regions[i].type = MemRegionPayload;
}
