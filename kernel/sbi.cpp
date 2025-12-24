#include <stdint.h>
#include <stdatomic.h>
#include <stdio.h>

#include "sbi.h"
#include "percpu.h"
#include "smp.h"
#include "utils.h"
#include "x86interrupts.h"

enum {
	SBI_SUCCESS = 0,
	SBI_ERR_NOT_SUPPORTED = -2,
	SBI_ERR_INVALID_PARAM = -3,
	SBI_ERR_ALREADY_AVAILABLE = -6,
};

enum {
	SBI_EXT_PUTC = 0x01,
	SBI_EXT_GETC = 0x02,
	SBI_EXT_BASE = 0x10,
	SBI_EXT_TIME = 0x54494D45,
	SBI_EXT_sPI  = 0x735049,
	SBI_EXT_RFNC = 0x52464E43,
	SBI_EXT_SRST = 0x53525354,
	SBI_EXT_HSM  = 0x48534d,
	SBI_EXT_PMU  = 0x504D55,
};

static uint64_t sbiCall(Hart *hart, uint64_t *result)
{
	uint64_t ext = hart->regs[17], func = hart->regs[16];
	switch (ext)
	{
	case SBI_EXT_BASE: // Base
		switch (func) {
		case 0: // SBI spec version
			*result = 1 << 24;
			return SBI_SUCCESS;
		case 1: // SBI implementation ID
			*result = 0x8086;
			return SBI_SUCCESS;
		case 2: // SBI implementation version
			*result = 0;
			return SBI_SUCCESS;
		case 3: // Probe SBI extension
			switch (hart->regs[10])
			{
			case SBI_EXT_BASE:
			case SBI_EXT_SRST:
			case SBI_EXT_HSM:
			case SBI_EXT_sPI:
			case SBI_EXT_RFNC:
				*result = 1;
				break;
			case SBI_EXT_TIME:
			case SBI_EXT_PMU:
				*result = 0;
				break;
			default:
				panic("SBI probed for unknown ext %lx", hart->regs[10]);
			}

			return SBI_SUCCESS;
		case 4: // Machine vendor ID
		case 5: // Machine arch ID
		case 6: // Machine implementation ID
			*result = 0;
			return SBI_SUCCESS;
		}
		break;
	// Legacy PUTC/GETC has a different ABI: R10 is the return value.
	case SBI_EXT_PUTC:
		putchar(hart->regs[10] & 0xFF);
		return 0; // Actually void
	case SBI_EXT_GETC:
		return ~0ul;
	case SBI_EXT_SRST:
		if (func == 0)
			panic("SBI system reset type %lx reason %lx", hart->regs[10], hart->regs[11]);

		break;
	case SBI_EXT_HSM:
		if (func == 0) { // hart_start
			uint64_t hartid = hart->regs[10], start_addr = hart->regs[11], opaque = hart->regs[12];

			int otherCPUNum = SMP::hartIDToCPUNum(hartid);
			if (otherCPUNum < 0)
				return SBI_ERR_INVALID_PARAM;

			auto *otherHart = &getPerCPUForOtherCPU(otherCPUNum)->hart;
			if (otherHart->state != Hart::State::STOPPED)
				return SBI_ERR_ALREADY_AVAILABLE; // The spec does not use SBI_ERR_ALREADY_STARTED here

			otherHart->mode = Hart::MODE_SUPERVISOR;
			otherHart->satp = 0;
			otherHart->sstatus = 0;
			otherHart->regs[10] = hartid;
			otherHart->regs[11] = opaque;
			otherHart->pc = start_addr;
			otherHart->state = Hart::State::START_PENDING;

			SMP::sendIPI(hartid, X86_IRQ_INTERNAL_IPI);
			return SBI_SUCCESS;
		}

		break;
	case SBI_EXT_sPI:
		if (func == 0) { // send_ipi
			uint64_t hart_mask = hart->regs[10], hart_mask_base = hart->regs[11];

			for (int hartBit = 0; hartBit < 64; ++hartBit) {
				if ((hart_mask & (1ul << hartBit)) == 0)
					continue;

				auto hartID = hart_mask_base + hartBit;
				int otherCPUNum = SMP::hartIDToCPUNum(hartID);
				if (otherCPUNum < 0)
					return SBI_ERR_INVALID_PARAM;

				SMP::sendIPI(hartID, X86_IRQ_RV_IPI);
			}

			return SBI_SUCCESS;
		}

		break;
	case SBI_EXT_RFNC:
		switch (func) {
		case 0: // remote_fence_i
			return SBI_SUCCESS; // Ignored for now
		case 1: // remote_sfence_vma
		case 2: // remote_sfence_vma_asid
		{
			uint64_t hart_mask = hart->regs[10], hart_mask_base = hart->regs[11],
			         start_addr = hart->regs[12], size = hart->regs[13],
			         asid = hart->regs[14];

			(void) asid; // Ignored for now

			for (int hartBit = 0; hartBit < 64; ++hartBit) {
				if ((hart_mask & (1ul << hartBit)) == 0)
					continue;

				auto hartID = hart_mask_base + hartBit;
				int otherCPUNum = SMP::hartIDToCPUNum(hartID);
				if (otherCPUNum < 0)
					return SBI_ERR_INVALID_PARAM;

				auto *otherHart = &getPerCPUForOtherCPU(otherCPUNum)->hart;

				// Try to set Idle -> Requesting
				for (;;) {
					Hart::RFenceState rfenceIdle = Hart::RFenceState::Idle;
					if (atomic_compare_exchange_weak(&otherHart->rfence_state, &rfenceIdle, Hart::RFenceState::Requesting))
						break;
					else
						asm volatile("pause");
				}

				// Set values
				otherHart->rfence_addr = start_addr;
				otherHart->rfence_size = size;
				// Set state to Requested
				otherHart->rfence_state = Hart::RFenceState::Requested;
				// Send IPI
				SMP::sendIPI(hartID, X86_IRQ_INTERNAL_IPI);

				// Wait for completion
				while (otherHart->rfence_state != Hart::RFenceState::Completed)
					asm volatile("pause");

				// Set back to Idle
				otherHart->rfence_state = Hart::RFenceState::Idle;
			}

			return SBI_SUCCESS;
		}
		default:
			break;
		}
		break;
	case 0x5D: // Used by riscv-tests
		if (hart->regs[10])
			panic("riscv-tests failed test %lu", hart->regs[10] >> 1);
		else
			panic("riscv-tests passed");

		break;
	}

	panic("SBI call ext %lx fun %lx not supported", ext, func);

	return SBI_ERR_NOT_SUPPORTED;
}

void handleSBICall(Hart *hart)
{
	uint64_t result = 0;
	hart->regs[10] = sbiCall(hart, &result);
	hart->regs[11] = result;
}
