#include "loaderapi.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

// TODO: per CPU?
static const size_t PROFILE_BUF_SIZE = 1024 * 1024;
static uint8_t profileBuf[PROFILE_BUF_SIZE];
static uint8_t *profileBufPtr = profileBuf;

extern "C" {
	void __cyg_profile_func_enter (void *this_fn, void *call_site);
	void __cyg_profile_func_exit  (void *this_fn, void *call_site);
}

bool profiling_enabled = false;

[[gnu::no_instrument_function]]
void profileBufOverflow() {
	profiling_enabled = false;

	const uint8_t *profileParsePtr = profileBuf;
	static unsigned int depth = 0;
	while (profileParsePtr < profileBufPtr) {
		uint64_t tscval;
		__builtin_memcpy(&tscval, profileParsePtr, sizeof(tscval));
		profileParsePtr += sizeof(tscval);
		if (tscval & 1) {
			depth--;
			for (int i = 0; i < depth; ++i)
				printf("\t");
			printf("Exit at %lu\n", tscval & ~1ul);
		} else {
			uint32_t funcptrLo;
			__builtin_memcpy(&funcptrLo, profileParsePtr, sizeof(funcptrLo));
			profileParsePtr += sizeof(funcptrLo);
			for (int i = 0; i < depth; ++i)
				printf("\t");
			printf("Enter %p at %lu\n", (void*) (KERNEL_LOAD_ADDR + funcptrLo), tscval);
			depth++;
		}
	}

	profiling_enabled = true;

	profileBufPtr = profileBuf;
}

[[gnu::no_instrument_function]]
static inline uint64_t rdtsc() {
	uint32_t low, high;
	asm volatile("rdtsc" : "=a" (low), "=d" (high));
	return (uint64_t(high) << 32) | low;
}

static struct {
	uint8_t *profilePtr;
	uint64_t tscval;
} lastCall;

static const uint64_t min_duration = 256;

[[gnu::no_instrument_function]]
void __cyg_profile_func_enter (void *this_fn, void *call_site) {
	if (!profiling_enabled)
		return;

	struct {
		uint64_t tscval;
		uint32_t funcptrLo;
	} __attribute__((packed)) entry { .tscval = rdtsc() & (~1ul), .funcptrLo = uint32_t(uintptr_t(this_fn) - KERNEL_LOAD_ADDR) };

	if (profileBufPtr + sizeof(entry) >= &profileBuf[PROFILE_BUF_SIZE])
		profileBufOverflow();

	lastCall.profilePtr = profileBufPtr;
	lastCall.tscval = entry.tscval;

	__builtin_memcpy(profileBufPtr, &entry, sizeof(entry));
	profileBufPtr += sizeof(entry);
}

[[gnu::no_instrument_function]]
void __cyg_profile_func_exit (void *this_fn, void *call_site) {
	if (!profiling_enabled)
		return;

	struct {
		uint64_t tscval;
	} entry { .tscval = rdtsc() | 1 };

	// If the function was too quick, don't bother saving it
	// and just rewind the pointer.
	if (lastCall.profilePtr == profileBufPtr - 12 && entry.tscval - lastCall.tscval < min_duration) {
		profileBufPtr = lastCall.profilePtr;
		return;
	}

	if (profileBufPtr + sizeof(entry) >= &profileBuf[PROFILE_BUF_SIZE])
		profileBufOverflow();

	__builtin_memcpy(profileBufPtr, &entry, sizeof(entry));
	profileBufPtr += sizeof(entry);
}
