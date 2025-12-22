#pragma once

#include <stdint.h>

// Frequency of the HPET in Hz
uint32_t hpetFrequency();

// Current value of the HPET timer
uint64_t hpetCurrentTime();

// Initialize HPET state from ACPI. To be run on CPU 0.
void setupHPET();

// To be run on all CPUs after setupHPET()
void setupLAPICTimer();

// Set time when to fire the LAPIC timer interrupt.
// Returns false if already expired or too close.
bool lapicSetTimeout(uint64_t hpetTarget);
