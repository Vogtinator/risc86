#pragma once

#include <stdint.h>

#include "utils.h"

#define SSTATUS_SIE (1ull << 1)
#define SSTATUS_SPIE (1ull << 5)
#define SSTATUS_SPP (1ull << 8)
#define SSTATUS_FS_MASK (3ul << 13)

#define SIP_SSIP (1ull << 1)
#define SIP_STIP (1ull << 5)
#define SIP_SEIP (1ull << 9)

struct Hart {
	// regs[0] is always 0, never written to
	uint64_t regs[32];
	uint64_t pc;

	// F/D regs
	union {
		double d;
		// 32-bit floats are NaN-boxed as 64-bit double,
		// represented by writing ~0 to the upper 32 bits.
		float f;
		struct { uint32_t low, high; } w;
		uint64_t x;
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
	_Atomic uint64_t sip; // Written by this CPU's IRQ handlers
	uint64_t sie;
	uint32_t scounteren; // Only & 3 supported
	uint64_t sscratch;
	uint64_t sepc;
	uint64_t scause;
	volatile uint64_t stval; // Written by this CPU's page fault handler
	uint64_t satp;
	uint64_t stimecmp;

	// AIA CSRs
	uint64_t stopi;
	uint64_t stopei;
	uint64_t siselect;

	// IMSIC CSRs (indirect)
	uint64_t eidelivery; // Only & 1 supported
	uint64_t eithreshold;
	_Atomic uint64_t eip_64[2]; // Written by this CPU's IRQ handlers
	uint64_t eie_64[2]; // 64 bits, so there is no eip1, eip3, ...

	// The _Atomic variables here are written by other CPUs
	// Same values as SBI HSM states
	_Atomic enum class State {
		STARTED = 0,
		STOPPED = 1,
		START_PENDING = 2,
	} state = Hart::State::STOPPED;

	// For implementing SBI RFENCE:
	// * At Idle initially
	// * SBI call on the sending hart sets it to Requesting
	// * Sending hart sets rfence_addr and rfence_size
	// * Sending hart sends an IPI
	// * Receiving hart acts and sets it to Completed
	// * Sending hart sets it back to Idle
	_Atomic enum class RFenceState {
		Idle = 0,
		Requesting,
		Requested,
		Completed,
	} rfence_state;
	_Atomic uint64_t rfence_addr, rfence_size;

	// For LR/SC emulation
	bool lr_sc_pending;
	uint64_t lr_sc_address, lr_sc_value;

	void dump();
	void run();

private:
	void handleInterrupt(uint64_t cause, uint64_t stval);
	void handlePendingInterrupts();
	void handleSRET();

	// Perform an instruction fetch of 16 bits at the given addr.
	// Returns false on fault.
	__attribute__((warn_unused_result))
	bool fetchInstruction(uint16_t *inst, uint64_t addr);
	// Tiny single-entry iTLB: Remember the last translated page
	struct {
		uint64_t last_virt, last_phys, last_satp;
	} itlb;

	// Read a T from guest virtual memory at addr.
	// On fault, prepares the CPU for fault handling and returns false.
	template <typename T> __attribute__((warn_unused_result))
	bool virtRead(uint64_t addr, T *value);

	// Get a pointer to T at addr in guest virtual memory.
	template <typename T> __attribute__((warn_unused_result))
	bool virtWritePtr(uint64_t addr, T **ptr);

	// Write a T to guest virtual memory.
	template <typename T> __attribute__((warn_unused_result))
	bool virtWrite(uint64_t addr, T value);

	inline void setFSDirty()
	{
		this->sstatus |= SSTATUS_FS_MASK;
	}

	inline uint64_t getReg(int r)
	{
		return this->regs[r];
	}

	inline void setReg(int r, uint64_t value)
	{
		if (r != 0)
			this->regs[r] = value;
	}

	// Templated getters and setters for floating-point registers
	template <typename T> T getFReg(int r);
	template <> float getFReg(int r)
	{
		if ((this->sstatus & SSTATUS_FS_MASK) == 0)
			panic("getFReg called with FS off!");

		return this->fregs[r].f;
	}

	template <> double getFReg(int r)
	{
		if ((this->sstatus & SSTATUS_FS_MASK) == 0)
			panic("getDReg called with FS off!");

		return this->fregs[r].d;
	}

	template <typename T> void setFReg(int r, T value);
	template <> void setFReg(int r, float value)
	{
		setFSDirty();
		this->fregs[r].f = value;
		this->fregs[r].w.high = ~0u;
	}
	template <> void setFReg(int r, double value)
	{
		setFSDirty();
		this->fregs[r].d = value;
	}

	uint32_t getFRegBitsFloat(int r)
	{
		if ((this->sstatus & SSTATUS_FS_MASK) == 0)
			panic("getDReg called with FS off!");

		return this->fregs[r].w.low;
	}

	uint64_t getFRegBitsDouble(int r)
	{
		if ((this->sstatus & SSTATUS_FS_MASK) == 0)
			panic("getDReg called with FS off!");

		return this->fregs[r].x;
	}

	void setFRegBitsFloat(int r, uint32_t value)
	{
		setFSDirty();
		this->fregs[r].w.low = value;
		this->fregs[r].w.high = ~0u;
	}

	void setFRegBitsDouble(int r, uint64_t value)
	{
		setFSDirty();
		this->fregs[r].x = value;
	}

	bool faultOnFSOff(uint32_t inst);
	uint64_t getCSR(uint16_t csr);
	void setCSR(uint16_t csr, uint64_t value);

	void runRVCInstruction(uint16_t inst);
	void runInstruction(uint32_t inst);
};
