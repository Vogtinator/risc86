#pragma once

#include "hart.h"

// There is one instance of this struct per cpu.
// The PerCpuState for a CPU can always be accessed
// by calling getPerCPU() on that CPU.
struct PerCpuState {
	struct Hart hart;
	unsigned int cpu_id;
};

void setupPerCPUState(unsigned int cpu_id);
__attribute__((no_caller_saved_registers))
PerCpuState *getPerCPU();
// Try not to make a mess with concurrent access.
PerCpuState *getPerCPUForOtherCPU(unsigned int cpuNum);
