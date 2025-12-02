#pragma once

#include <stdint.h>

struct HartState {
	// regs[0] is always 0, never written to
	uint64_t regs[32];
	uint64_t pc;
	// CSRs
	uint64_t sstatus;
	uint64_t sie, sip;
	uint64_t satp;
	uint32_t scounteren; // Only & 3 supported
};
