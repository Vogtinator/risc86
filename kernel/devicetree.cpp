#include <stdio.h>
#include <libfdt.h>

#include "devicetree.h"

PhysAddr buildDeviceTreeBlob()
{
	const size_t dtb_maxsize = 8 * 1024;
	const PhysAddr dt_phys = physMemMgr.allocate(dtb_maxsize, MemRegionPayload);
	void * const dt_virt = phys_to_virt<void>(dt_phys);

	// TODO: Error checking
	// Using the sequential write API would be more efficient,
	// but the full RW API makes it more obvious what's happening.
	fdt_create_empty_tree(dt_virt, dtb_maxsize);

	// Set up root node
	const int root_ofs = 0;
	fdt_setprop_u32(dt_virt, root_ofs, "#address-cells", 2);
	fdt_setprop_u32(dt_virt, root_ofs, "#size-cells", 2);
	fdt_setprop_string(dt_virt, root_ofs, "compatible", "risc86");

	// Chosen node
	const int chosen_ofs = fdt_add_subnode(dt_virt, root_ofs, "chosen");
	fdt_setprop_string(dt_virt, chosen_ofs, "bootargs", "debug loglevel=9 earlycon=sbi keep_bootcon");

	// CPUs subnode
	const int cpus_ofs = fdt_add_subnode(dt_virt, root_ofs, "cpus");
	fdt_setprop_u32(dt_virt, cpus_ofs, "#address-cells", 1);
	fdt_setprop_u32(dt_virt, cpus_ofs, "#size-cells", 0);
	fdt_setprop_u32(dt_virt, cpus_ofs, "timebase-frequency", 1 * 1024 * 1024); // TODO

	// TODO: SMP
	for (int ncpu = 0; ncpu < 1; ++ncpu) {
		char cpu_nodename[] = "cpu@XXX";
		snprintf(cpu_nodename, sizeof(cpu_nodename), "cpu@%d", ncpu);
		const int cpu_ofs = fdt_add_subnode(dt_virt, cpus_ofs, cpu_nodename);
		fdt_setprop_string(dt_virt, cpu_ofs, "device-type", "cpu");
		fdt_setprop_u32(dt_virt, cpu_ofs, "reg", ncpu);
		fdt_setprop_string(dt_virt, cpu_ofs, "status", "okay");
		fdt_setprop_string(dt_virt, cpu_ofs, "compatible", "riscv");
		fdt_setprop_string(dt_virt, cpu_ofs, "riscv,isa-base", "rv64i");
		for (const char *ext : (const char*[]){"i", "m", "a", "c", "svade", "sstc", "zicsr", "zifencei"})
			fdt_appendprop_string(dt_virt, cpu_ofs, "riscv,isa-extensions", ext);

		fdt_setprop_string(dt_virt, cpu_ofs, "mmu-type", "riscv,sv39");

		const int cpu_intc_ofs = fdt_add_subnode(dt_virt, cpu_ofs, "interrupt-controller");
		fdt_setprop_u32(dt_virt, cpu_intc_ofs, "#interrupt-cells", 1);
		fdt_setprop_string(dt_virt, cpu_intc_ofs, "compatible", "riscv,cpu-intc");
		fdt_setprop_empty(dt_virt, cpu_intc_ofs, "interrupt-controller");
	}

	// Mark all remainig free memory as used by the guest
	physMemMgr.markRestForPayload();

	// Add memory nodes
	physMemMgr.iteratePayloadRegions([&] (auto &region) {
		if (region.type == MemRegionPayload) {
			char mem_nodename[] = "memory@XXXXXXXXXXXXXXXX";
			snprintf(mem_nodename, sizeof(mem_nodename), "memory@%lx", region.start);
			const int mem_ofs = fdt_add_subnode(dt_virt, root_ofs, mem_nodename);
			fdt_setprop_string(dt_virt, mem_ofs, "device_type", "memory");
			fdt_appendprop_u64(dt_virt, mem_ofs, "reg", region.start);
			fdt_appendprop_u64(dt_virt, mem_ofs, "reg", region.end - region.start + 1);
		}
	});

	printf("DTB size: %u B\n", fdt_totalsize(dt_virt));

	fdt_pack(dt_virt); // TODO: Needed?

	printf("DTB size: %u B\n", fdt_totalsize(dt_virt));

	return dt_phys;
}
