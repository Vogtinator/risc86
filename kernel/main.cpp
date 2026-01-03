#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "acpi.h"
#include "devicetree.h"
#include "hart.h"
#include "hpet.h"
#include "loaderapi.h"
#include "mem.h"
#include "pci.h"
#include "percpu.h"
#include "sbi.h"
#include "smp.h"
#include "utils.h"
#include "x86interrupts.h"

// Run global constructors defined in .init_array.
// _init_array_{start,end} are provided by kernel.ld.
using Constructor = void(*)(void);
extern "C" { extern Constructor _init_array_start[], _init_array_end; }

static void runGlobalConstructors()
{
	for(int i = 0; uintptr_t(&_init_array_start[i]) < uintptr_t(&_init_array_end); ++i)
		_init_array_start[i]();
}

extern "C" __attribute__((noreturn)) __attribute__((section(".text.entry")))
void kernel_entry(KernelParams *params)
{
	kernel_params = *params;
	puts("Starting kernel...");

	runGlobalConstructors();

	setupGDT(0);

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

	// Reserve a trampoline page early to ensure it's in lowmem.
	auto trampolinePage = physMemMgr.allocate(PAGE_SIZE, MemRegionTrampoline);

	// Set up per-CPU state for the boot CPU. Must come after GDT setup.
	setupPerCPUState(0);

	setupACPI();

	PCI::setupPCI();

	setupInterrupts();

	X86MMU::initGlobal();

	setupHPET();

	for (unsigned int i = 0; i < MAX_CPUS; ++i)
		getPerCPUForOtherCPU(i)->x86mmu.init();

	auto secondaryCallback = [](unsigned int cpuNum) {
		setupGDT(cpuNum);

		// Setup per-CPU state for secondary cores. Must come after GDT setup.
		setupPerCPUState(cpuNum);

		setupInterruptsPerCPU();

		setupLAPICTimer();

		getPerCPU()->x86mmu.initPerCPU();

		auto *hart = &getPerCPU()->hart;

		for (;;) {
			while (hart->state != Hart::State::STARTED)
				asm volatile("hlt");

			hart->run();
		}
	};

	// Before MMU context takeover: This needs identity mapping for the trampoline.
	SMP::setupSMP(trampolinePage, secondaryCallback);

	setupLAPICTimer();

	setupSBI();

	getPerCPU()->x86mmu.initPerCPU();

	printf("Free mem: %lu MiB\n", physMemMgr.totalFreeBytes() / 1024 / 1024);

	PhysAddr dtb = buildDeviceTreeBlob();
	printf("Device tree @ %lx\n", dtb);

	kernel_virt = phys_to_virt<uint8_t>(dtb);
	printf("DTB %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);

	printf("Memory map at exit:\n");
	physMemMgr.print();

	// Set up hart 0 to jump to the kernel
	auto *hart0 = &getPerCPU()->hart;
	hart0->state = Hart::State::STARTED;
	hart0->mode = Hart::MODE_SUPERVISOR;
	hart0->regs[10] = SMP::cpuNumToHartID(0); // a0 = Hart ID
	hart0->regs[11] = dtb; // a1 = phys addr of DT
	hart0->pc = kernel_params.kernel_phys;

	hart0->run();

	panic("Kernel exited");

	for(;;)
		asm volatile("cli; hlt");
}
