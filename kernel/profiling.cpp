#include "loaderapi.h"
#include <stdint.h>
#include <stddef.h>

bool profiling_enabled = false;

struct [[gnu::packed]] ProfileEvent {
	// TSC value. Bit 0 clear means call, bit 0 set means return.
	uint64_t tscval;
	// Called function. Offset from KERNEL_LOAD_ADDR, only low 32 bits.
	uint32_t funcOffset;
};

// TODO: per CPU?
static const size_t PROFILE_BUF_SIZE = 1024 * 1024;
static ProfileEvent profileBuf[PROFILE_BUF_SIZE];
static size_t profileEvents = 0;

[[gnu::no_instrument_function]]
static void profileBufOverflow() {
	uint8_t *profileBufBytes = reinterpret_cast<uint8_t*>(&profileBuf);
	size_t profileBufByteCount = profileEvents * sizeof(ProfileEvent);
	for (size_t i = 0; i < profileBufByteCount; ++i)
		asm volatile("out %[c], %[port]" :: [port] "d" (uint16_t(0x3f8)), [c] "a" (*profileBufBytes++));

	profileEvents = 0;
}

[[gnu::no_instrument_function]]
static inline uint64_t rdtsc() {
	uint32_t low, high;
	asm volatile("rdtsc" : "=a" (low), "=d" (high));
	return (uint64_t(high) << 32) | low;
}

static const uint64_t min_duration = 1024;

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
