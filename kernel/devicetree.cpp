#include <libfdt.h>

#include "devicetree.h"

const size_t dtb_maxsize = 8 * 1024;

PhysAddr buildDeviceTreeBlob()
{
	PhysAddr dt_phys = physMemMgr.allocate(dtb_maxsize, MemRegionPayload);
	void *dt_virt = phys_to_virt<void>(dt_phys);

	// TODO: Error checking
	fdt_create_empty_tree(dt_virt, dtb_maxsize);

	fdt_pack(dt_virt); // TODO: Needed?

	return dt_phys;
}
