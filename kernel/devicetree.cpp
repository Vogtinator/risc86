#include <stdio.h>
#include <libfdt.h>

#include "devicetree.h"
#include "hpet.h"
#include "loaderapi.h"
#include "pci.h"
#include "percpu.h"
#include "smp.h"

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
	// TODO: Instead of irqchip.riscv_imsic_noipi, implement IMSIC IPIs
	fdt_setprop_string(dt_virt, chosen_ofs, "bootargs", "loglevel=9 earlycon=sbi console=hvc0 security=selinux systemd.log_target=console linuxrc.log=/dev/console linuxrc.debug=1 pci=realloc rw root=/dev/vdb2 init=/usr/lib/systemd/systemd irqchip.riscv_imsic_noipi");
	// TODO: Fill that randomly (using rdrand/rdseed?)
	for(uint32_t i : (uint32_t[]){0xe3c3d5c1u, 0x67270453u, 0x27a09781u, 0xa54ed241u,
	                              0x66d189d4u, 0x24efaf2fu, 0xf887c4d7u, 0xec6a7c2bu})
		fdt_appendprop_u32(dt_virt, chosen_ofs, "rng-seed", i);

	if (kernel_params.initrd_len) {
		fdt_setprop_u64(dt_virt, chosen_ofs, "linux,initrd-start", kernel_params.initrd_phys);
		fdt_setprop_u64(dt_virt, chosen_ofs, "linux,initrd-end", kernel_params.initrd_phys + kernel_params.initrd_len);
	}

	// CPUs subnode
	const int cpus_ofs = fdt_add_subnode(dt_virt, root_ofs, "cpus");
	fdt_setprop_u32(dt_virt, cpus_ofs, "#address-cells", 1);
	fdt_setprop_u32(dt_virt, cpus_ofs, "#size-cells", 0);
	fdt_setprop_u32(dt_virt, cpus_ofs, "timebase-frequency", 1000000);

	uint32_t cpu_intc_phandles[MAX_CPUS] = {};

	for (unsigned int ncpu = 0; ncpu < SMP::numberOfCPUs(); ++ncpu) {
		char cpu_nodename[] = "cpu@XXX";
		snprintf(cpu_nodename, sizeof(cpu_nodename), "cpu@%d", ncpu);
		const int cpu_ofs = fdt_add_subnode(dt_virt, cpus_ofs, cpu_nodename);
		fdt_setprop_string(dt_virt, cpu_ofs, "device_type", "cpu");
		fdt_setprop_u32(dt_virt, cpu_ofs, "reg", SMP::cpuNumToHartID(ncpu));
		fdt_setprop_string(dt_virt, cpu_ofs, "status", "okay");
		fdt_setprop_string(dt_virt, cpu_ofs, "compatible", "riscv");
		fdt_setprop_string(dt_virt, cpu_ofs, "riscv,isa-base", "rv64i");
		for (const char *ext : (const char*[]){"i", "m", "a", "f", "d", "c", "ssaia", "svade", "sstc", "zicsr", "zifencei"})
			fdt_appendprop_string(dt_virt, cpu_ofs, "riscv,isa-extensions", ext);

		fdt_setprop_string(dt_virt, cpu_ofs, "mmu-type", "riscv,sv39");

		const int cpu_intc_ofs = fdt_add_subnode(dt_virt, cpu_ofs, "interrupt-controller");
		fdt_generate_phandle(dt_virt, &cpu_intc_phandles[ncpu]);
		fdt_setprop_u32(dt_virt, cpu_intc_ofs, "phandle", cpu_intc_phandles[ncpu]);
		fdt_setprop_u32(dt_virt, cpu_intc_ofs, "#interrupt-cells", 1);
		fdt_setprop_string(dt_virt, cpu_intc_ofs, "compatible", "riscv,cpu-intc");
		fdt_setprop_empty(dt_virt, cpu_intc_ofs, "interrupt-controller");
	}

	// Add LAPIC as IMSIC :D
	const int imsic_ofs = fdt_add_subnode(dt_virt, root_ofs, "lapic");
	uint32_t imsic_phandle;
	fdt_generate_phandle(dt_virt, &imsic_phandle);
	fdt_setprop_u32(dt_virt, imsic_ofs, "phandle", imsic_phandle);
	fdt_setprop_string(dt_virt, imsic_ofs, "compatible", "riscv,imsics");
	fdt_setprop_empty(dt_virt, imsic_ofs, "interrupt-controller");
	fdt_setprop_u32(dt_virt, imsic_ofs, "#interrupt-cells", 0);
	fdt_setprop_empty(dt_virt, imsic_ofs, "msi-controller");
	fdt_setprop_u32(dt_virt, imsic_ofs, "#msi-cells", 0);
	fdt_appendprop_u64(dt_virt, imsic_ofs, "reg", 0xFEE00000ul);
	fdt_appendprop_u64(dt_virt, imsic_ofs, "reg", 0x1000ul);
	fdt_setprop_u32(dt_virt, imsic_ofs, "riscv,num-ids", 63);
	// TODO: Mention other CPUs, has to match LAPIC IDs for MSIs
	for (int ncpu = 0; ncpu < 1; ++ncpu) {
		fdt_appendprop_u32(dt_virt, imsic_ofs, "interrupts-extended", cpu_intc_phandles[ncpu]);
		fdt_appendprop_u32(dt_virt, imsic_ofs, "interrupts-extended", 9);
	}

	// Mention all PCIe controllers
	for (uint32_t i = 0; i < PCI::numControllers; ++i) {
		auto *controller = &PCI::controllers[i];
		char pcie_nodename[] = "pcie@XXXXXXXXXXXXXXXX";
		snprintf(pcie_nodename, sizeof(pcie_nodename), "pcie@%lx", controller->ecam);
		const int pcie_ofs = fdt_add_subnode(dt_virt, root_ofs, pcie_nodename);
		fdt_setprop_string(dt_virt, pcie_ofs, "compatible", "pci-host-ecam-generic");
		fdt_setprop_string(dt_virt, pcie_ofs, "device_type", "pci");
		fdt_setprop_u32(dt_virt, pcie_ofs, "#address-cells", 3);
		fdt_setprop_u32(dt_virt, pcie_ofs, "#interrupt-cells", 1);
		fdt_setprop_u32(dt_virt, pcie_ofs, "#size-cells", 2);
		fdt_setprop_u32(dt_virt, pcie_ofs, "msi-parent", imsic_phandle);
		fdt_appendprop_u32(dt_virt, pcie_ofs, "bus-range", controller->startBus);
		fdt_appendprop_u32(dt_virt, pcie_ofs, "bus-range", controller->endBus);
		fdt_appendprop_u64(dt_virt, pcie_ofs, "reg", controller->ecam);
		fdt_appendprop_u64(dt_virt, pcie_ofs, "reg", (controller->endBus + 1) << 20);

		for (uint32_t r = 0; r < controller->numRanges; ++r) {
			auto *range = &controller->ranges[r];
			uint32_t flags = 0;
			if (range->prefetchable)
				flags |= 1 << 30;
			if (range->is64bit)
				flags |= 0b11 << 24;
			else
				flags |= 0b10 << 24;

			fdt_appendprop_u32(dt_virt, pcie_ofs, "ranges", flags);
			fdt_appendprop_u64(dt_virt, pcie_ofs, "ranges", range->start);
			fdt_appendprop_u64(dt_virt, pcie_ofs, "ranges", range->start);
			fdt_appendprop_u64(dt_virt, pcie_ofs, "ranges", range->size);
		}
	}

	if (kernel_params.fb.pitch) {
		printf("Have framebuffer at %p\n", (void*)kernel_params.fb.phys);
		int fb_ofs = fdt_add_subnode(dt_virt, root_ofs, "fb");
		fdt_setprop_string(dt_virt, fb_ofs, "status", "okay");
		fdt_setprop_string(dt_virt, fb_ofs, "compatible", "simple-framebuffer");
		fdt_appendprop_u64(dt_virt, fb_ofs, "reg", kernel_params.fb.phys);
		fdt_appendprop_u64(dt_virt, fb_ofs, "reg", kernel_params.fb.pitch * kernel_params.fb.height);
		fdt_setprop_u32(dt_virt, fb_ofs, "width", kernel_params.fb.width);
		fdt_setprop_u32(dt_virt, fb_ofs, "height", kernel_params.fb.height);
		fdt_setprop_u32(dt_virt, fb_ofs, "stride", kernel_params.fb.pitch);
		fdt_setprop_string(dt_virt, fb_ofs, "format", "x8r8g8b8");
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
