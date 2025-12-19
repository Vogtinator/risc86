#pragma once

#include <stdint.h>

void lapicWrite(uint32_t offset, uint32_t value);
uint32_t lapicRead(uint32_t offset);
void markRVInterruptHandled(unsigned int rvExtIRQ);
void setupInterrupts();
