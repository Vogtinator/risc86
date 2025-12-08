#include <stdio.h>

#include "sbi.h"
#include "utils.h"

enum {
	SBI_SUCCESS = 0,
	SBI_ERR_NOT_SUPPORTED = -2,
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
				*result = 1;
				break;
			case SBI_EXT_TIME:
			case SBI_EXT_sPI:
			case SBI_EXT_RFNC:
			case SBI_EXT_HSM:
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
