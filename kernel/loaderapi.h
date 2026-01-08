#pragma once

#include <stddef.h>
#include <stdint.h>

// Each possible core allocates X86JIT::JIT_REGION_SIZE B (128MB ATM),
// which can be substantial. Can be raised once that's done for online
// CPUs only.
#define MAX_CPUS 6

// Define the virtual memory layout
static const uint64_t
	// Stack for CPUx begins at KERNEL_STACK_LOW + (x * KERNEL_STACK_CPU_OFFSET)
	KERNEL_STACK_LOW  = 0xFFFF800000000000UL,
	KERNEL_STACK_SIZE = 1 * 1024 * 1024,     // 1 MiB
	KERNEL_STACK_CPU_OFFSET  = KERNEL_STACK_SIZE * 2,

	KERNEL_LOAD_ADDR  = 0xFFFF810000000000UL,
	KERNEL_PHYS_START = 0xFFFF900000000000UL,
	KERNEL_PHYS_END   = 0xFFFFA00000000000UL;

enum MemoryRegionType {
	MemRegionFree = 0, // Free for any purpose
	MemRegionPayload,  // Used for passed kernel and initrd, must be preserved
	// Reserved memory is just omitted from the list

	// Following types are used during runtime
	MemRegionTrampoline, // Used for SMP startup
	MemRegionPageTables, // Used for host paging structures
	MemRegionJIT, // JIT translated host code
};

// Packed to avoid ABI mismatches between EFI and Kernel
struct KernelParams {
	uint64_t kernel_phys, kernel_len;
	uint64_t initrd_phys, initrd_len;
	struct {
		uint64_t phys;
		uint32_t width, height, pitch, bpp;
	} fb;
	uint64_t xsdp_phys;
	size_t memory_region_count;
	struct {
		uint64_t start, size;
		enum MemoryRegionType type;
	} memory_regions[512];
} __attribute__ ((packed));
