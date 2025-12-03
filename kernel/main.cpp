#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "cpu.h"
#include "devicetree.h"
#include "mem.h"
#include "loaderapi.h"
#include "percpu.h"
#include "utils.h"

KernelParams kernel_params;

__attribute__((noreturn)) __attribute__((section(".text.entry"))) void kernel_entry(KernelParams *params)
{
	kernel_params = *params;
	puts("Starting kernel...");

	// Set up per-CPU state for the boot CPU
	setupPerCPUState(0);

	printf("Kernel @ %lx len %lx\n", params->kernel_phys, params->kernel_len);
	printf("Initrd @ %lx len %lx\n", params->initrd_phys, params->initrd_len);

	uint8_t *kernel_virt = phys_to_virt<uint8_t>(params->kernel_phys);
	printf("Kernel %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);
	kernel_virt = phys_to_virt<uint8_t>(params->initrd_phys);
	printf("Initrd %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);

	for(size_t i = 0; i < params->memory_region_count; ++i)
		physMemMgr.addRegion(params->memory_regions[i].start, params->memory_regions[i].start + params->memory_regions[i].size - 1, params->memory_regions[i].type);

	printf("Memory map at boot:\n");
	physMemMgr.print();

	// Hack: Reserve the zero page so that the first allocation is not at physical 0
	// and some invalid pointers are more obvious. TODO: Or is it fine if the DTB
	// just lives there?
	physMemMgr.allocate(4096, MemRegionZeroPage);

	printf("Free mem: %lu MiB\n", physMemMgr.totalFreeBytes() / 1024 / 1024);

	PhysAddr dtb = buildDeviceTreeBlob();
	printf("Device tree @ %lx\n", dtb);

	kernel_virt = phys_to_virt<uint8_t>(dtb);
	printf("DTB %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);

	printf("Memory map at exit:\n");
	physMemMgr.print();

	// Set up hart 0 to jump to the kernel
	auto *hart0 = &getPerCPU()->hart;
	hart0->regs[10] = 0; // a0 = Hart ID
	hart0->regs[11] = dtb; // a1 = phys addr of DT
	hart0->pc = params->kernel_phys;

	runThisCPU();

	panic("Kernel exited");

	for(;;)
		asm volatile("cli; hlt");
}
