#include <stdio.h>

#include "pci.h"
#include "mem.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"

PCI::Controller PCI::controllers[PCI::MAX_CONTROLLERS];
uint8_t PCI::numControllers = 0;

static PCI::Controller::Range getBARRange(PhysAddr funcEcam, int bar)
{
	PCI::Controller::Range ret = {};

	// Read the current BAR configurtion
	volatile uint32_t *barPtr = phys_to_virt<volatile uint32_t>(funcEcam + 0x10 + 4 * bar);
	uint64_t oldBar = barPtr[0];

	if (oldBar & 0b1) {
		printf("Ignoring IO BAR%d\n", bar);
		return ret;
	}

	if ((oldBar & 0b110) == 0b100)
		ret.is64bit = true;

	if (oldBar & 0b1000)
		ret.prefetchable = true;

	if (ret.is64bit && bar == 5)
		panic("BAR 5 can't be 64 bit");

	if (ret.is64bit)
		oldBar |= uint64_t(barPtr[1]) << 32;

	ret.start = oldBar & ~0b1111ul;

	// Get the size of the BAR

	volatile uint32_t *cmdPtr = phys_to_virt<volatile uint32_t>(funcEcam + 0x4);

	// Disable memory BARs
	uint32_t oldCmd = *cmdPtr;
	if (oldCmd & 0b10)
		*cmdPtr = oldCmd & ~0b10;

	// Set all bits in the BAR
	barPtr[0] = ~0;
	if (ret.is64bit)
		barPtr[1] = ~0;

	// Read which address bits remain set
	uint64_t barAddrBits = barPtr[0] & ~0b1111ul;
	if (ret.is64bit)
		barAddrBits |= uint64_t(barPtr[1]) << 32;
	else
		barAddrBits |= ~0ul << 32;

	// Restore previous value
	barPtr[0] = oldBar;
	if (ret.is64bit)
		barPtr[1] = oldBar >> 32;

	// Reenable memory BARs (if necessary)
	*cmdPtr = oldCmd;

	ret.size = ~barAddrBits + 1;

	return ret;
}

// Scan the PCI bus for all BARs and MSI(-X) ranges that need to be intercepted.
// The device tree needs to specify the memory ranges available to PCI devices
// (This could be determined through ACPI, but needs full AML...)
static void scanBus(PCI::Controller *controller, int bus)
{
	for (int device = 0; device < 32; ++device) {
		for (int function = 0; function < 8; ++function) {
			PhysAddr funcEcam = controller->ecam + (bus << 20) + (device << 15) + (function << 12);
			uint32_t pciFuncId = *phys_to_virt<uint32_t>(funcEcam);
			if (pciFuncId == 0xFFFFFFFFu)
				break;

			printf("PCI function at %02x.%02x.%d: %04x:%04x\n", bus, device, function, pciFuncId & 0xFFFF, pciFuncId >> 16);

			uint8_t headerType = *phys_to_virt<uint32_t>(funcEcam + 0xc) >> 16;

			if ((headerType & 0x7F) == 1)
				panic("PCI bridge found, not handled");

			if ((headerType & 0x7F) != 0)
				panic("Unknown PCI header type %x", headerType);

			// Iterate all BARs
			for (int bar = 0; bar <= 5; ++bar) {
				auto range = getBARRange(funcEcam, bar);
				if (range.start == 0)
					continue;

				printf("BAR%d from %lx-%lx\n", bar, range.start, range.start + range.size);

				if (range.is64bit) // Covers two BARs
					++bar;

				if (controller->numRanges >= PCI::MAX_RANGES_PER_CONTROLLER)
					panic("Too many ranges");

				controller->ranges[controller->numRanges] = range;
				controller->numRanges++;
			}

			if (function == 0 && !(headerType & 0x80)) // Not a multi function device?
				break; // Skip scanning functions 1-7
		}
	}
}

void PCI::setupPCI()
{
	uacpi_table tbl;
	if (uacpi_table_find_by_signature("MCFG", &tbl) != UACPI_STATUS_OK)
		panic("No MCFG table");

	// Scan all buses mentioned in the MCFG
	acpi_mcfg *mcfg = reinterpret_cast<acpi_mcfg*>(tbl.ptr);
	uint32_t numEntries = (mcfg->hdr.length - sizeof(*mcfg)) / sizeof(mcfg->entries[0]);
	for (uint32_t i = 0; i < numEntries; ++i) {
		auto *entry = &mcfg->entries[i];
		if (entry->segment != 0)
			panic("PCI segment groups not implemented");

		if (numControllers >= PCI::MAX_CONTROLLERS)
			panic("Too many PCI host controllers");

		auto *controller = &PCI::controllers[numControllers];
		numControllers++;
		controller->ecam = entry->address;
		controller->startBus = entry->start_bus;
		controller->endBus = entry->end_bus;

		for (int bus = entry->start_bus; bus <= entry->end_bus; ++bus)
			scanBus(controller, bus);
	}
}
