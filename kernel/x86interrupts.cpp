#include <stdint.h>
#include <stdio.h>

#include "x86interrupts.h"
#include "mem.h"
#include "percpu.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"

// Frame as pushed by the CPU and passed to
// functions with __attribute__((interrupt))
struct InterruptFrame
{
	uint64_t ip;
	uint64_t cs;
	uint64_t flags;
	uint64_t sp;
	uint64_t ss;
};

// x86 can't use vectors 0-31 for external interrupts,
// so shift RISC-V interrupts by 32.
__attribute__((no_caller_saved_registers))
static uint8_t x86IRQtoRVExtIRQ(uint8_t x86IRQ)
{
	// TODO: Kernel patched to not need shifting.
	return x86IRQ;
	//return x86IRQ - X86_IRQ_RV_FIRST;
}

static uint8_t rvExtIRQtoX86IRQ(unsigned int rvIRQ)
{
	if (rvIRQ < X86_IRQ_RV_FIRST || rvIRQ >= X86_IRQ_RV_LAST)
		panic("Invalid RV IRQ %d", rvIRQ);

	// TODO: Kernel patched to not need shifting.
	return rvIRQ;
	//return x86IRQ + X86_IRQ_RV_FIRST;
}

__attribute__((no_caller_saved_registers))
static void irqHandler(InterruptFrame *frame, int irq)
{
	auto *hart = &getPerCPU()->hart;

	if (irq == X86_IRQ_LAPIC_TIMER) {
		hart->sip |= SIP_STIP;
		lapicWrite(0xB0, 0x00);
	} else if (irq >= X86_IRQ_RV_FIRST && irq <= X86_IRQ_RV_LAST) {
		static_assert(X86_IRQ_RV_LAST / 64 < sizeof(Hart::eip_64)/sizeof(Hart::eip_64[0]));
		auto rvExtIRQ = x86IRQtoRVExtIRQ(irq);
		if (rvExtIRQ / 64 >= sizeof(Hart::eip_64)/sizeof(Hart::eip_64[0]))
			panic("Unexpected IRQ %d", irq);

		hart->eip_64[rvExtIRQ / 64] |= 1ul << (rvExtIRQ % 64);
	} else if (irq == X86_IRQ_RV_IPI) {
		hart->sip |= SIP_SSIP;
		lapicWrite(0xB0, 0x00);
	} else if (irq == X86_IRQ_INTERNAL_IPI) {
		if (hart->state == Hart::State::START_PENDING)
			hart->state = Hart::State::STARTED;
		else
			panic("IPI for unknown cause\n");

		lapicWrite(0xB0, 0x00);
	} else if (irq == X86_IRQ_SPURIOUS) {
		// lapicWrite(0xB0, 0x00); needed?
		return;
	} else {
		panic("Unexpected IRQ %d", irq);
	}
}

// x86 calls a different handler for each interrupt vector,
// with no (generic) way to get the number dynamically.
// This is a template to generate wrappers for each IRQ that
// dispatch to a common handler function.
template <int irq> __attribute__((interrupt))
static void irqHandler(InterruptFrame *frame)
{
	return irqHandler(frame, irq);
}

__attribute__((interrupt))
static void doubleFaultHandler(InterruptFrame *frame, uint64_t errorCode)
{
	(void) errorCode;
	// TODO: Use a panic function here that doesn't ud2 at the end
	// This will clobber registers, but it's FUBAR anyway at this point.
	panic("Double fault at 0x%lx!", frame->ip);
}

struct IDTEntry {
	uint16_t offset_low = 0;
	uint16_t cs = 0;
	uint8_t ist = 0;
	uint8_t flags = 0;
	uint16_t offset_mid = 0;
	uint32_t offset_high = 0;
	uint32_t reserved = 0;

	template <typename HandlerType>
	explicit IDTEntry (HandlerType handler) :
	    offset_low(uintptr_t(handler)),
	    cs(SegmentKernelCS),
	    ist(0), // TODO: Separate interrupt stack to allow red zone for non-handlers again?
	    flags(0b11101110), // Catch-all: Present interrupt gate accessible to Ring 3
	    offset_mid(uintptr_t(handler) >> 16),
	    offset_high(uintptr_t(handler) >> 32),
	    reserved(0)
	{}

	IDTEntry() {}
} __attribute__ ((packed));

static IDTEntry idt[] = {
    [0x08] = IDTEntry{doubleFaultHandler},
#define IRQ_HANDLER(n) [n] = IDTEntry{irqHandler<n>}
#define IRQ_HANDLERS_2(n) IRQ_HANDLER(n), IRQ_HANDLER(n+1)
#define IRQ_HANDLERS_4(n) IRQ_HANDLERS_2(n), IRQ_HANDLERS_2(n+2)
#define IRQ_HANDLERS_8(n) IRQ_HANDLERS_4(n), IRQ_HANDLERS_4(n+4)
#define IRQ_HANDLERS_16(n) IRQ_HANDLERS_8(n), IRQ_HANDLERS_8(n+8)
#define IRQ_HANDLERS_32(n) IRQ_HANDLERS_16(n), IRQ_HANDLERS_16(n+16)
#define IRQ_HANDLERS_64(n) IRQ_HANDLERS_32(n), IRQ_HANDLERS_32(n+32)
    IRQ_HANDLERS_32(0x20),
    IRQ_HANDLERS_32(0x40), IRQ_HANDLERS_32(0x60),
    IRQ_HANDLERS_32(0x80), IRQ_HANDLERS_32(0xA0),
    IRQ_HANDLERS_32(0xC0), IRQ_HANDLERS_32(0xE0),
};

static PhysAddr lapicPhys = 0;
static volatile uint32_t *lapic = nullptr;

__attribute__((no_caller_saved_registers))
void lapicWrite(uint32_t offset, uint32_t value)
{
	lapic[offset / 4] = value;
}

uint32_t lapicRead(uint32_t offset)
{
	return lapic[offset / 4];
}

static void outb(uint16_t port, uint8_t value)
{
	asm volatile("out %[c], %[port]" :: [port] "d" (port), [c] "a" (value));
}

void setupInterrupts()
{
	outb(0x21, 0xFF); // Mask master PIC interrupts

	uacpi_table tbl;
	if (uacpi_table_find_by_signature("APIC", &tbl) != UACPI_STATUS_OK)
		panic("MADT not found");

	acpi_madt *madt = reinterpret_cast<acpi_madt*>(tbl.ptr);
	lapicPhys = madt->local_interrupt_controller_address;

	auto cb = [] (uacpi_handle user, acpi_entry_hdr *hdr) {
		(void) user;
		if (hdr->type == ACPI_MADT_ENTRY_TYPE_LAPIC_ADDRESS_OVERRIDE) {
			auto lapic_tbl = reinterpret_cast<acpi_madt_lapic_address_override*>(hdr);
			lapicPhys = lapic_tbl->address;
			return UACPI_ITERATION_DECISION_BREAK;
		}

		return UACPI_ITERATION_DECISION_CONTINUE;
	};

	uacpi_for_each_subtable(&madt->hdr, sizeof(*madt), cb, NULL);

	printf("LAPIC addr: %lx\n", lapicPhys);
	lapic = phys_to_virt<volatile uint32_t>(lapicPhys);

	setupInterruptsPerCPU();
}

void setupInterruptsPerCPU()
{
	struct {
		uint16_t size;
		uintptr_t idtAddr;
	} __attribute__((packed)) idtr = {
		.size = sizeof(idt) - 1,
		.idtAddr = uintptr_t(&idt),
	};

	asm volatile("lidt %[idtr]" :: [idtr] "m" (idtr));

	// Set the spurious interrupt register to 0x100 | 0xFF
	lapicWrite(0xF0, 0x1FF);

	asm volatile("sti");
}

void markRVInterruptHandled(unsigned int rvExtIRQ)
{
	unsigned int x86IRQ = rvExtIRQ;

	getPerCPU()->hart.eip_64[rvExtIRQ / 64] &= ~(1ul << (rvExtIRQ % 64));

	// TODO: How to ensure there's no race?
	lapicWrite(0xB0, 0x00);
}
