#include <stdint.h>
#include <stdio.h>

#include "x86interrupts.h"
#include "mem.h"
#include "percpu.h"
#include "rvmmu.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"

// Frame as pushed by the CPU and passed to
// functions with __attribute__((interrupt)).
// Members have to be volatile, otherwise writes
// get optimized away.
struct InterruptFrame
{
	volatile uint64_t ip;
	volatile uint64_t cs;
	volatile uint64_t flags;
	volatile uint64_t sp;
	volatile uint64_t ss;
};

// x86 can't use vectors 0-31 for external interrupts,
// so shift RISC-V interrupts by 32.
CALLED_FROM_IRQ
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

CALLED_FROM_IRQ
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
		// TODO: EOI should only be sent in markRVExtInterruptHandled(),
		// but interrupts are enabled so by then another higher prio IRQ might've taken over
		// and the EOI acks the wrong one.
		lapicWrite(0xB0, 0x00);
	} else if (irq == X86_IRQ_RV_IPI) {
		hart->sip |= SIP_SSIP;
		// TODO: Like above, EOI should only be sent in markRVIPIHandled().
		lapicWrite(0xB0, 0x00);
	} else if (irq == X86_IRQ_INTERNAL_IPI) {
		if (hart->state == Hart::State::START_PENDING)
			hart->state = Hart::State::STARTED;
		else if (hart->rfence_state == Hart::RFenceState::Requested) {
			if (hart->rfence_size == ~0ul || (hart->rfence_addr == 0 && hart->rfence_size == 0))
				getPerCPU()->x86mmu.resetContext();
			else
				getPerCPU()->x86mmu.flushRVMapping(hart->rfence_addr, hart->rfence_size);

			hart->rfence_state = Hart::RFenceState::Completed;
		} else
			panic("IPI for unknown cause\n");

		lapicWrite(0xB0, 0x00);
	} else if (irq == X86_IRQ_SPURIOUS) {
		return;
	} else {
		panic("Unexpected IRQ %d", irq);
	}
}

__attribute__((naked))
static void switchRing0Handler()
{
	// When returning with iretq, the CPU only switches stacks if there's
	// a difference in rings, but this returns from Ring 0 to Ring 0,
	// so the stack has to be switched manually.
	// Instead of using iretq, just return normally by making a stack frame
	// on the caller's stack (!) and switching to it.
	asm(R"asm(
	    mov %rax, -0x08(%rsp) # Save RAX and RBX for later
	    mov %rbx, -0x10(%rsp)
	    movw $0x10, %ax # Reload KernelSegmentDS into %ss
	    mov %ax, %ss
	    mov 0x18(%rsp), %rbx # Load the previous RSP
	    sub $0x18, %rbx
	    mov -0x10(%rsp), %rax # Get the the old value of RBX
	    mov %rax, 0x00(%rbx) # And put it onto the caller's stack
	    mov 0x10(%rsp), %rax # Load the previous RFLAGS
	    mov %rax, 0x08(%rbx) # Write RFLAGS to it
	    mov 0(%rsp), %rax # Load the previous RIP
	    mov %rax, 0x10(%rbx) # Write RIP to it
	    mov -0x08(%rsp), %rax # Restore RAX
	    mov %rbx, %rsp # Switch stacks
	    pop %rbx
	    popfq
	    ret
	)asm");
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

__attribute__((interrupt)) __attribute__((target("general-regs-only")))
static void pageFaultHandler(InterruptFrame *frame, uint64_t errorCode)
{
	// Get the fault address from CR2
	uint64_t addr;
	asm("mov %%cr2, %[addr]" : [addr] "=r" (addr));

	uint64_t sign = int64_t(addr) >> 39;
	if ((sign != 0 && ~sign != 0) || (errorCode & ~0b111))
		panic("Unexpected page fault (code 0x%lx) for address %lx at %lx", errorCode, addr, frame->ip);

	bool pagePresent = errorCode & 0b001, isWrite = errorCode & 0b010, isUser = errorCode & 0b100;

	auto *hart = &getPerCPU()->hart;
	auto translationResult = mmu_translate(hart, addr, isWrite ? AccessType::Write : AccessType::Read);

	bool isFault = false;

	if (translationResult.pageoff_mask) {
		getPerCPU()->x86mmu.addRVMapping(addr, &translationResult);
	} else {
		isFault = true;
	}

	// Set the Carry flag on fault and advance, clear it otherwise
	if (isFault) {
		// Find which instruction caused the fault to get its length.
		// Proper decoding not needed here, the set of possible instructions
		// is known.
		const uint64_t faultInsn = *(uint64_t*)(frame->ip);
		unsigned int faultInsnLen;
		if ((faultInsn & 0xFFFFFF) == 0x028b48) // mov (%rdx), %rax
			faultInsnLen = 3;
		else if ((faultInsn & 0xFFFF) == 0x028b) // mov (%rdx), %eax
			faultInsnLen = 2;
		else if ((faultInsn & 0xFFFFFF) == 0x028b66) // mov (%rdx), %ax
			faultInsnLen = 3;
		else if ((faultInsn & 0xFFFF) == 0x028a) // mov (%rdx), %al
			faultInsnLen = 2;
		else if ((faultInsn & 0xFFFFFF) == 0x028948) // mov %rax, (%rdx)
			faultInsnLen = 3;
		else if ((faultInsn & 0xFFFF) == 0x0289) // mov %eax, (%rdx)
			faultInsnLen = 2;
		else if ((faultInsn & 0xFFFFFF) == 0x028966) // mov %ax, (%rdx)
			faultInsnLen = 3;
		else if ((faultInsn & 0xFFFF) == 0x0288) // mov %al, (%rdx)
			faultInsnLen = 2;
		else
			panic("Unexpected instruction in fault handler: %lx at %lx", faultInsn, frame->ip);

		// Skip the faulting instruction and set the
		frame->ip += faultInsnLen;
		frame->flags |= 1ul << 0;
	} else {
		// Retry the instruction.
		// Carry should be clear already.
		// frame->flags &= ~(1ul << 0);
	}

	return;
}

__attribute__((interrupt)) __attribute__((target("general-regs-only")))
static void gpFaultHandler(InterruptFrame *frame, uint64_t errorCode)
{
	panic("General Protection Fault at %lx, code %lx", frame->ip, errorCode);
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
	    ist(0),
	    flags(0b11101110), // Catch-all: Present interrupt gate accessible to Ring 3
	    offset_mid(uintptr_t(handler) >> 16),
	    offset_high(uintptr_t(handler) >> 32),
	    reserved(0)
	{}

	IDTEntry() {}
} __attribute__ ((packed));

static const IDTEntry idt[] = {
    [0x08] = IDTEntry{doubleFaultHandler},
    [0x0d] = IDTEntry{gpFaultHandler},
    [0x0e] = IDTEntry{pageFaultHandler},
#define IRQ_HANDLER(n) [n] = IDTEntry{irqHandler<n>}
#define IRQ_HANDLERS_2(n) IRQ_HANDLER(n), IRQ_HANDLER(n+1)
#define IRQ_HANDLERS_4(n) IRQ_HANDLERS_2(n), IRQ_HANDLERS_2(n+2)
#define IRQ_HANDLERS_8(n) IRQ_HANDLERS_4(n), IRQ_HANDLERS_4(n+4)
#define IRQ_HANDLERS_16(n) IRQ_HANDLERS_8(n), IRQ_HANDLERS_8(n+8)
#define IRQ_HANDLERS_32(n) IRQ_HANDLERS_16(n), IRQ_HANDLERS_16(n+16)
#define IRQ_HANDLERS_64(n) IRQ_HANDLERS_32(n), IRQ_HANDLERS_32(n+32)
    IRQ_HANDLERS_32(0x20),
    IRQ_HANDLERS_64(0x40),
    [X86_IRQ_RING_0] = IDTEntry{switchRing0Handler},
    IRQ_HANDLERS_64(0xC0),
};

static PhysAddr lapicPhys = 0;
static volatile uint32_t *lapic = nullptr;

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

void markRVIPIHandled()
{
	// TODO: How to ensure there's no race?
	//lapicWrite(0xB0, 0x00);
}

void markRVExtInterruptHandled(unsigned int rvExtIRQ)
{
	unsigned int x86IRQ = rvExtIRQ;

	getPerCPU()->hart.eip_64[rvExtIRQ / 64] &= ~(1ul << (rvExtIRQ % 64));

	// TODO: How to ensure there's no race?
	//lapicWrite(0xB0, 0x00);
}

struct GDTEntry {
	uint16_t limit_lo;
	uint16_t base_lo;
	uint8_t base_mid;
	uint8_t access;
	uint8_t flags_limit;
	uint8_t base_hi;
} __attribute__((packed));

struct GDT {
	GDTEntry entries[7];
};

static const GDT gdtTemplate = {
    .entries = {
        [0] = {},
        [SegmentKernelCS/8] = { .limit_lo = 0xFFFF, .access = 0b10011011, .flags_limit = 0b10101111 }, // Ring 0 CS
        [SegmentKernelDS/8] = { .limit_lo = 0xFFFF, .access = 0b10010011, .flags_limit = 0b10001111 }, // Ring 0 DS
        [SegmentUserCS/8] = { .limit_lo = 0xFFFF, .access = 0b11111011, .flags_limit = 0b10101111 }, // Ring 3 CS
        [SegmentUserDS/8] = { .limit_lo = 0xFFFF, .access = 0b11110011, .flags_limit = 0b10001111 }, // Ring 3 DS
        [SegmentTSS/8] = { .access = 0b10001001, .flags_limit = 0b00000000 }, // TSS (1st half)
        [SegmentTSS/8 + 1] = { }, // TSS (2nd half)
    },
};

static GDT gdt[MAX_CPUS];

static struct {
	uint32_t rsv0;
	uint64_t rsp[3];
	uint64_t rsv1;
	uint64_t ist[7];
	uint64_t rsv2;
	uint16_t rsv3;
	uint16_t iobpPtr;
} __attribute__((packed)) tss[MAX_CPUS];

void setupGDT(unsigned int cpuNum)
{
	gdt[cpuNum] = gdtTemplate;

	// Create the TSS for this CPU: Only needs rsp[0] set for interrupt handling.
	// Use lowest 16KiB of the main stack for this CPU.
	tss[cpuNum].rsp[0] = KERNEL_STACK_LOW + (cpuNum * KERNEL_STACK_CPU_OFFSET) + 16 * 1024;
	tss[cpuNum].iobpPtr = sizeof(tss[cpuNum]);

	// Set the TSS segment descriptor in the GDT
	uintptr_t tssAddr = uintptr_t(&tss[cpuNum]);
	gdt[cpuNum].entries[SegmentTSS/8].base_lo = tssAddr;
	gdt[cpuNum].entries[SegmentTSS/8].base_mid = tssAddr >> 16;
	gdt[cpuNum].entries[SegmentTSS/8].base_hi = tssAddr >> 24;
	gdt[cpuNum].entries[SegmentTSS/8 + 1].limit_lo = tssAddr >> 32;
	gdt[cpuNum].entries[SegmentTSS/8 + 1].base_lo = tssAddr >> 48;
	gdt[cpuNum].entries[SegmentTSS/8].access = 0b10001001;
	gdt[cpuNum].entries[SegmentTSS/8].limit_lo = sizeof(tss[cpuNum]) - 1;

	struct {
		uint16_t limit;
		uintptr_t pointer;
	} __attribute__((packed)) gdtp = {
	    .limit = sizeof(gdt[cpuNum]) - 1,
	    .pointer = uintptr_t(&gdt[cpuNum]),
	};

	// Load GDT
	asm volatile("lgdt %[gdtp]" :: [gdtp] "m" (gdtp));

	// Load TSS
	asm volatile("ltr %[tssSeg]" :: [tssSeg] "r" (uint16_t(SegmentTSS)));

	// Use lretq to far jump to the new CS
	asm volatile("push %[cs]\n"
	             "leaq 1f(%%rip), %%rax\n"
	             "pushq %%rax\n"
	             "lretq\n"
	             "1:\n"
	             :: [cs] "i" (SegmentKernelCS) : "flags", "memory", "rax");

	// Reload data segment registers.
	// User segments also work in Ring 0, so use them throughout Ring 0 and 3
	// execution, which also leaves FSBASE intact. SS has to be DPL=0 though.
	asm volatile("mov %[ds], %%ds\n"
	             "mov %[ds], %%es\n"
	             "mov %[ds], %%fs\n"
	             "mov %[ds], %%gs\n"
	             "mov %[ss], %%ss\n"
	             :: [ds] "r" (SegmentUserDS), [ss] "r" (SegmentKernelDS) : "memory");
}
