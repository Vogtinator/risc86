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
