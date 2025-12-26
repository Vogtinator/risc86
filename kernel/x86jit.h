#pragma once

#include "hart.h"

class X86JIT
{
public:
	// Allocate memory for the generated code
	void init();

	// Try to generate (if necessary) and run code for the current hart state.
	// Returns false if fallback to the interpreter is neccessary.
	bool tryJit(Hart *hart);

	// Discard all translations.
	void reset();
private:
	// Region where the generated code is stored and executed from
	uint8_t *codeRegionStart, *codeRegionEnd;
	// Watermark between used and free parts of the code region.
	// The next instruction will be written here.
	uint8_t *codeRegionCurrent;
};
