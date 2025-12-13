#include "hpet.h"
#include "mem.h"
#include "stdio.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"

#define HPET_REG_CAPS 0x000
#define HPET_REG_CONF 0x010
#define HPET_REG_INT 0x020
#define HPET_REG_VAL 0x0F0
#define HPET_REG_TIMER_CONF(x) 0x100+x*0x20
#define HPET_REG_TIMER_COMP(x) 0x108+x*0x20

static volatile uint64_t *hpetRegs;

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

void setupHPET()
{
	uacpi_table tbl;
	if (uacpi_table_find_by_signature("HPET", &tbl) != UACPI_STATUS_OK)
		panic("HPET table not found");

	acpi_hpet *hpetTbl = reinterpret_cast<acpi_hpet*>(tbl.ptr);
	hpetRegs = phys_to_virt<volatile uint64_t>(hpetTbl->address.address);
	printf("HPET frequency: %u Hz\n", hpetFrequency());

	// Set enable bit
	auto conf = hpetRead(HPET_REG_CONF);
	conf |= 1;
	hpetWrite(HPET_REG_CONF, conf);
}
