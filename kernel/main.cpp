#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#include "loaderapi.h"

template <typename T>
static T *phys_to_virt(uint64_t addr)
{
	return reinterpret_cast<T*>(KERNEL_PHYS_START + addr);
}

__attribute__((noreturn)) __attribute__((section(".text.entry"))) void kernel_entry(KernelParams *params)
{
	puts("Starting kernel...");

	printf("Kernel @%lx len %lx\n", params->kernel_phys, params->kernel_len);
	printf("Initrd @%lx len %lx\n", params->initrd_phys, params->initrd_len);

	uint8_t *kernel_virt = phys_to_virt<uint8_t>(params->kernel_phys);
	printf("Kernel %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);
	kernel_virt = phys_to_virt<uint8_t>(params->initrd_phys);;
	printf("Initrd %02x%02x%02x%02x%02x%02x%02x%02x\n", kernel_virt[0], kernel_virt[1], kernel_virt[2], kernel_virt[3], kernel_virt[4], kernel_virt[5], kernel_virt[6], kernel_virt[7]);

	size_t total = 0;
	puts("Memory map:");
	for(size_t i = 0; i < params->memory_region_count; ++i) {
		printf("Entry %02ld %09lx-%09lx %08d\n", i, params->memory_regions[i].start, params->memory_regions[i].start + params->memory_regions[i].size, params->memory_regions[i].type);
		if(params->memory_regions[i].type == MemRegionFree || params->memory_regions[i].type == MemRegionPayload)
			total += params->memory_regions[i].size;
	}

	printf("Total usable mem: %ld MiB\n", total / 1024 / 1024);
	for(;;)
		asm volatile("cli; hlt");
}
