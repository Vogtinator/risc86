#pragma once

#include <stdint.h>

// general-regs-only shouldn't be necessary, but there's some issue
// with a misaligned stack which causes movaps to fault.
#define CALLED_FROM_IRQ __attribute__((no_caller_saved_registers)) __attribute__((target("general-regs-only")))

enum x86IRQ {
	// Mapped 1:1 to RV external interrupts
	X86_IRQ_RV_FIRST = 32,
	X86_IRQ_RV_LAST = 95,
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

// Segments defined by the GDT
enum GDTSegment {
	SegmentKernelCS = 0x8,
	SegmentKernelDS = 0x10,
	SegmentUserCS = 0x18 | 3,
	SegmentUserDS = 0x20 | 3,
	SegmentTSS = 0x28,
};

void setupGDT(unsigned int cpuNum);
