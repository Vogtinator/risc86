#pragma once

#include <stdint.h>

// Frequency of the HPET in Hz
uint32_t hpetFrequency();

// Current value of the HPET timer
uint64_t hpetCurrentTime();

// Initialize HPET state from ACPI
void setupHPET();
