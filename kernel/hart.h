#pragma once

#include <stdint.h>

#define SSTATUS_SIE (1ull << 1)
#define SSTATUS_SPIE (1ull << 5)
#define SSTATUS_SPP (1ull << 8)

struct HartState {
	// regs[0] is always 0, never written to
	uint64_t regs[32];
	uint64_t pc;

	enum {
		MODE_USER = 0,
		MODE_SUPERVISOR = 1,
	} mode;

	enum {
		SCAUSE_ECALL_UMODE = 8,
		SCAUSE_ECALL_SMODE = 9,
		SCAUSE_INSTR_PAGE_FAULT = 12,
		SCAUSE_LOAD_PAGE_FAULT = 13,
		SCAUSE_STORE_PAGE_FAULT = 15,
	};

	// CSRs
	uint64_t sstatus;
	uint64_t stvec;
	uint64_t sip, sie;
	uint32_t scounteren; // Only & 3 supported
	uint64_t sscratch;
	uint64_t sepc;
	uint64_t scause;
	uint64_t stval;
	uint64_t satp;
};
