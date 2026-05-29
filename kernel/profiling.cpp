#include "loaderapi.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

bool profiling_enabled = false;

struct [[gnu::packed]] ProfileEvent {
	uint64_t tscval;
	uint32_t funcOffset;
};

// TODO: per CPU?
static const size_t PROFILE_BUF_SIZE = 1024 * 1024;
static ProfileEvent profileBuf[PROFILE_BUF_SIZE];
static size_t profileEvents = 0;

[[gnu::no_instrument_function]]
static void profileBufOverflow() {
	profiling_enabled = false;

	static unsigned int depth = 0;
	for (int i = 0; i < profileEvents; ++i) {
		ProfileEvent *event = &profileBuf[i];
		if (event->tscval & 1) {
			depth--;
			for (int i = 0; i < depth; ++i)
				printf("\t");
			printf("Exit at %lu\n", event->tscval & ~1ul);
		} else {
			for (int i = 0; i < depth; ++i)
				printf("\t");
			printf("Enter %p at %lu\n", (void*) (KERNEL_LOAD_ADDR + event->funcOffset), event->tscval);
			depth++;
		}
	}

	profiling_enabled = true;

	profileEvents = 0;
}

[[gnu::no_instrument_function]]
static inline uint64_t rdtsc() {
	uint32_t low, high;
	asm volatile("rdtsc" : "=a" (low), "=d" (high));
	return (uint64_t(high) << 32) | low;
}

static const uint64_t min_duration = 256;

extern "C" [[gnu::no_instrument_function]]
void __cyg_profile_func_enter (void *this_fn, void *call_site) {
	if (!profiling_enabled)
		return;

	if (profileEvents == PROFILE_BUF_SIZE)
		profileBufOverflow();

	profileBuf[profileEvents++] = {
	    .tscval = rdtsc() & (~1ul),
	    .funcOffset = uint32_t(uintptr_t(this_fn) - KERNEL_LOAD_ADDR)
	};
}

extern "C" [[gnu::no_instrument_function]]
void __cyg_profile_func_exit (void *this_fn, void *call_site) {
	if (!profiling_enabled)
		return;

	ProfileEvent event {
		.tscval = rdtsc() | 1,
		.funcOffset = uint32_t(uintptr_t(this_fn) - KERNEL_LOAD_ADDR)
	};

	// If the function was too quick, skip recording events.
	if (profileEvents > 0
	    && profileBuf[profileEvents - 1].funcOffset == event.funcOffset
	    && !(profileBuf[profileEvents - 1].tscval & 1)
	    && event.tscval - profileBuf[profileEvents - 1].tscval <= min_duration) {
		profileEvents--;
		return;
	}

	if (profileEvents == PROFILE_BUF_SIZE)
		profileBufOverflow();

	profileBuf[profileEvents++] = event;
}
