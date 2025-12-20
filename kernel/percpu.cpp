#include <stdint.h>

#include "percpu.h"
#include "loaderapi.h"
#include "utils.h"

/* Per-CPU state handling: %fs is set up to point at the CPU's
 * entry in perCpuStatePtrs, so that %fs:(0) ends up being a pointer
 * to its PerCpuState in perCpuStates. */

static PerCpuState perCpuStates[MAX_CPUS];
static PerCpuState *perCpuStatePtrs[MAX_CPUS];

void setupPerCPUState(unsigned int cpu_id)
{
	if (cpu_id >= MAX_CPUS)
		panic("cpu_id %d >= MAX_CPUS (%d)", cpu_id, MAX_CPUS);

	perCpuStates[cpu_id].cpu_id = cpu_id;
	perCpuStatePtrs[cpu_id] = &perCpuStates[cpu_id];

	// Set the %fs base
	uintptr_t ptr = uintptr_t(&perCpuStatePtrs[cpu_id]);
	uint32_t ptr_lo = uint32_t(ptr), ptr_hi = ptr >> 32;
	asm volatile("wrmsr" :: "a" (ptr_lo), "d" (ptr_hi), "c" (0xc0000100) : "memory");
}

__attribute__((no_caller_saved_registers))
PerCpuState *getPerCPU()
{
	PerCpuState *ret;
	asm volatile("mov %%fs:(0), %[ret]" : [ret] "=r" (ret));
	return ret;
}

PerCpuState *getPerCPUForOtherCPU(unsigned int cpuNum)
{
	if (cpuNum >= MAX_CPUS)
		panic("cpuNum %d >= MAX_CPUS (%d)", cpuNum, MAX_CPUS);

	return &perCpuStates[cpuNum];
}
