#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

struct KernelParams {
	uint64_t kernel_phys, kernel_len;
	uint64_t initrd_phys, initrd_len;
	struct MemoryAttrib {
		uint64_t start, size;
		uint64_t flags;
	} memory_regions[256];
} __attribute__ ((packed));

__attribute__((noreturn)) __attribute__((section(".text.entry"))) void kernel_entry(KernelParams *params)
{
	puts("Starting kernel...");

	puts("Memory map:");
	for(int i = 0; i < 42; ++i)
		printf("Entry %02d %09lx-%09lx %08lx\n", i, params->memory_regions[i].start, params->memory_regions[i].size, params->memory_regions[i].flags);

	for(;;)
		asm volatile("cli; hlt");
}
