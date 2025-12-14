#pragma once

#include "mem.h"
#include <stdint.h>

namespace PCI {

static const int MAX_CONTROLLERS = 16, MAX_RANGES_PER_CONTROLLER = 64;

struct Controller {
	PhysAddr ecam;
	uint8_t startBus, endBus;
	struct Range {
		bool is64bit, prefetchable;
		PhysAddr start, size;
	} ranges[MAX_RANGES_PER_CONTROLLER];
	uint8_t numRanges;
};

extern Controller controllers[MAX_CONTROLLERS];

extern uint8_t numControllers;

void setupPCI();

}
