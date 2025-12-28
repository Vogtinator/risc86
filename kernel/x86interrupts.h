#pragma once

#include <stdint.h>

// general-regs-only shouldn't be necessary, but there's some issue
// with a misaligned stack which causes movaps to fault.
#define X86_IRQ_HANDLER __attribute__((interrupt)) __attribute__((target("general-regs-only")))
// This should be __attribute__((no_caller_saved_registers)), but this triggers some LLVM bugs:
// With -O0: https://github.com/llvm/llvm-project/issues/173800
// Without LTO: functions that return structs clobber %rax which the callers don't expect.
// Use __attribute__((target("general-regs-only"))) instead and require -O1
#define CALLED_FROM_IRQ __attribute__((target("general-regs-only")))
#if defined(__clang__) && !defined(__OPTIMIZE__)
#error Must turn on optimizations to avoid clang bugs
#endif

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

enum x86IRQ {
	// x86 CPU internal vectors
	X86_IRQ_DOUBLE_FAULT = 8,
	X86_IRQ_GP_FAULT = 13,
	X86_IRQ_PAGE_FAULT = 14,
	// Mapped 1:1 to RV external interrupts
	X86_IRQ_RV_FIRST = 32,
	X86_IRQ_RV_LAST = 95,
	// Switch to Ring 0
	X86_IRQ_RING_0 = 128,
	// LAPIC timer
	X86_IRQ_LAPIC_TIMER = 252,
	// Mapped to RV supervisor software interrupts
	X86_IRQ_RV_IPI = 253,
	// For internal purposes like SBI functions
	X86_IRQ_INTERNAL_IPI = 254,
	X86_IRQ_SPURIOUS = 255,
};

CALLED_FROM_IRQ void lapicWrite(uint32_t offset, uint32_t value);
uint32_t lapicRead(uint32_t offset);
void markRVExtInterruptHandled(unsigned int rvExtIRQ);
void markRVIPIHandled();
void setupInterrupts();
void setupInterruptsPerCPU();

void installIRQHandler(uint8_t irq, void *handler);

// Segments defined by the GDT
enum GDTSegment {
	SegmentKernelCS = 0x8,
	SegmentKernelDS = 0x10,
	SegmentUserCS = 0x18 | 3,
	SegmentUserDS = 0x20 | 3,
	SegmentTSS = 0x28,
};

// Set up GDT and TSS
void setupGDT(unsigned int cpuNum);
