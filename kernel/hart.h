#pragma once

#include <stdint.h>

#define SSTATUS_SIE (1ull << 1)
#define SSTATUS_SPIE (1ull << 5)
#define SSTATUS_SPP (1ull << 8)
#define SSTATUS_FS_MASK (3ul << 13)

#define SIP_STIP (1ull << 5)

struct HartState {
	// regs[0] is always 0, never written to
	uint64_t regs[32];
	uint64_t pc;

	// F/D regs
	union {
		double d;
		// 32-bit floats are NaN-boxed as 64-bit double,
		// represented by writing ~0 to the upper 32 bits.
		float f;
		struct { uint32_t low, high; } u;
	} fregs[32];
	uint64_t fcsr;

	enum {
		MODE_USER = 0,
		MODE_SUPERVISOR = 1,
	} mode;

	enum {
		SCAUSE_ILLEGAL_INSTRUCTION = 2,
		SCAUSE_EBREAK = 3,
		SCAUSE_LOAD_MISALIGN = 4,
		SCAUSE_STORE_MISALIGN = 6,
		SCAUSE_ECALL_UMODE = 8,
		SCAUSE_ECALL_SMODE = 9,
		SCAUSE_INSTR_PAGE_FAULT = 12,
		SCAUSE_LOAD_PAGE_FAULT = 13,
		SCAUSE_STORE_PAGE_FAULT = 15,
		SCAUSE_INTERRUPT_BASE = 1ull << 63,
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
	uint64_t stimecmp;
};
