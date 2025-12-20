#pragma once

#include <stdint.h>

enum x86IRQ {
	X86_IRQ_RV_FIRST = 32,
	X86_IRQ_RV_LAST = 95,
	X86_IRQ_IPI = 254,
	X86_IRQ_SPURIOUS = 255,
};

void lapicWrite(uint32_t offset, uint32_t value);
uint32_t lapicRead(uint32_t offset);
void markRVInterruptHandled(unsigned int rvExtIRQ);
void setupInterrupts();
void setupInterruptsPerCPU();
