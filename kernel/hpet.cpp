#include "hpet.h"
#include "mem.h"
#include "percpu.h"
#include "stdio.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"
#include "x86interrupts.h"

#define HPET_REG_CAPS 0x000
#define HPET_REG_CONF 0x010
#define HPET_REG_INT 0x020
#define HPET_REG_VAL 0x0F0
#define HPET_REG_TIMER_CONF(x) 0x100+x*0x20
#define HPET_REG_TIMER_COMP(x) 0x108+x*0x20

// To convert HPET diffs to LAPIC diffs. 48.16 fixed point format.
// TODO: Is this really globally constant?
static uint64_t hpetTicksToLAPICTicksMultiplier;
static volatile uint64_t *hpetRegs;
// This may have to be chosen dynamically for a good ratio?
static const int lapicTimerDivRegVal = 0b001; // Divide by 4.

static uint64_t hpetRead(unsigned int offset)
{
	return hpetRegs[offset / 8];
}

static void hpetWrite(unsigned int offset, uint64_t value)
{
	hpetRegs[offset / 8] = value;
}

uint32_t hpetFrequency()
{
	// Upper 32bit are interval in femtoseconds
	return 1000000000000000ul / (hpetRead(HPET_REG_CAPS) >> 32);
}

uint64_t hpetCurrentTime()
{
	return hpetRead(HPET_REG_VAL);
}

static uint64_t hpetTicksToLAPICTicks(uint64_t ticks)
{
	// TODO: Handle overflow somehow?
	return (ticks * hpetTicksToLAPICTicksMultiplier) >> 16;
}

static void hpetSleep(uint64_t count)
{
	uint64_t target = hpetCurrentTime() + count;
	while (hpetCurrentTime() < target) asm volatile("pause");
}

bool lapicSetTimeout(uint64_t hpetTarget)
{
	lapicWrite(0x380, 0); // Stop timer

	if (hpetTarget == ~0ull)
		return true; // Linux writes all ones to disable it.

	uint64_t hpetCurrent = hpetCurrentTime();
	if (hpetTarget <= hpetCurrent)
		return false;

	uint64_t hpetDiff = hpetTarget - hpetCurrent;
	if (hpetDiff > INT32_MAX)
		 hpetDiff = INT32_MAX;

	uint64_t lapicDiff = hpetTicksToLAPICTicks(hpetDiff);
	if (lapicDiff == 0)
		return false;

	if (lapicDiff >= UINT32_MAX) {
		// TODO: Handle this properly?
		printf("Too many ticks, cutting short\n");
		lapicDiff = ~0ul;
	}

	lapicWrite(0x380, lapicDiff); // Start timer
	return true;
}

static void calibrateLAPIC()
{
	// Figure out the LAPIC timer frequency
	printf("Calibrating LAPIC timer...\n");

	lapicWrite(0x3E0, lapicTimerDivRegVal); // Set up divider
	lapicWrite(0x320, (1 << 16)); // One shot mode, interrupt masked
	lapicWrite(0x380, ~0u); // Set timer to highest value
	hpetSleep(hpetFrequency() / 100); // Wait for 10ms (TODO: Recheck actual time diff?)

	// Calculate the multiplier using the LAPIC diff
	uint32_t lapicDiff = ~0u - lapicRead(0x390);
	hpetTicksToLAPICTicksMultiplier = uint64_t(lapicDiff) * (1 << 16) / (hpetFrequency() / 100);

	printf("1000 HPET ticks map to %lu LAPIC ticks\n", hpetTicksToLAPICTicks(1000));
}

X86_IRQ_HANDLER
static void lapicIRQHandler(InterruptFrame *frame)
{
	(void) frame;
	auto *hart = &getPerCPU()->hart;
	hart->sip |= SIP_STIP;
	lapicWrite(0xB0, 0x00);
}

void setupHPET()
{
	uacpi_table tbl;
	if (uacpi_table_find_by_signature("HPET", &tbl) != UACPI_STATUS_OK)
		panic("HPET table not found");

	acpi_hpet *hpetTbl = reinterpret_cast<acpi_hpet*>(tbl.ptr);
	hpetRegs = phys_to_virt<volatile uint64_t>(hpetTbl->address.address);
	printf("HPET frequency: %u Hz\n", hpetFrequency());

	// TODO: 32-bit HPETs are rather common, so implementing wraparound may be necessary
	if (!(hpetRead(HPET_REG_CAPS) & (1 << 13)))
		panic("HPET does not have 64bit counter");

	auto conf = hpetRead(HPET_REG_CONF);
	conf |= 0b01; // Enable counter
	hpetWrite(HPET_REG_CONF, conf);

	calibrateLAPIC();

	installIRQHandler(X86_IRQ_LAPIC_TIMER, (void*) lapicIRQHandler);
}

void setupLAPICTimer()
{
	lapicWrite(0x3E0, lapicTimerDivRegVal); // Set up divider
	lapicWrite(0x380, ~0); // Set timer value to 0 (stop)
	lapicWrite(0x320, X86_IRQ_LAPIC_TIMER); // One shot mode, timer IRQ
}
