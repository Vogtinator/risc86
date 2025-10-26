#include "utils.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

// If panic triggers a panic itself, it attempts less risky ways of panicing.
static int in_panic = 0;

void panic(const char *fmt, ...)
{
	in_panic += 1;

	if (in_panic == 1) {
		va_list va;
		va_start(va, fmt);
		fprintf(stderr, "\n\nPANIC at %p: ", __builtin_return_address(0));
		vfprintf(stderr, fmt, va);
		fprintf(stderr, "\n");
		va_end(va);
	} else if (in_panic == 2) {
		static const char msg[] = "\n\nRECURSIVE PANIC: ";
		write(2, msg, sizeof(msg) - 1);
		write(2, fmt, strlen(fmt));
		write(2, "\n", 1);
	}

	__builtin_trap();
}
