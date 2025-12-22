#pragma once

#include <stdint.h>

#include "mem.h"

namespace SMP {
extern unsigned int numberOfCPUs();
uint64_t cpuNumToHartID(unsigned int cpuNum);
int hartIDToCPUNum(uint64_t hartID); // Returns -1 on not found
void setupSMP(PhysAddr trampolinePage, void secondaryCallback(unsigned int cpuNum));
void sendIPI(uint64_t hartId, uint8_t irq);
}
