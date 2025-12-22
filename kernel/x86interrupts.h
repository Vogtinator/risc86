#pragma once

#include <stdint.h>

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

__attribute__((no_caller_saved_registers))
void lapicWrite(uint32_t offset, uint32_t value);
uint32_t lapicRead(uint32_t offset);
void markRVExtInterruptHandled(unsigned int rvExtIRQ);
void markRVIPIHandled();
void setupInterrupts();
void setupInterruptsPerCPU();

// Segments defined by the GDT
enum GDTSegment {
	SegmentKernelCS = 0x8,
	SegmentKernelDS = 0x10,
	SegmentUserCS = 0x18,
	SegmentUserDS = 0x20,
};

void setupGDT();
