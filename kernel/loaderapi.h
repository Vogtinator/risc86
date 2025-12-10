#pragma once

#include <stddef.h>
#include <stdint.h>

// Define the virtual memory layout
static const uint64_t
	KERNEL_STACK_LOW  = 0xFFFF800000000000UL,
	KERNEL_STACK_SIZE = 12 * 1024 * 1024,     // 12MiB
	KERNEL_LOAD_ADDR  = 0xFFFF810000000000UL,
	KERNEL_PHYS_START = 0xFFFF900000000000UL,
	KERNEL_PHYS_END   = 0xFFFFA00000000000UL;

// Segments defined by the GDT
enum GDTSegment {
	SegmentKernelCS = 1,
	SegmentKernelDS,
	SegmentUserCS,
	SegmentUserDS,
};

enum MemoryRegionType {
	MemRegionFree = 0, // Free for any purposse
	MemRegionPayload,  // Used for passed kernel and initrd, must be preserved
	MemRegionZeroPage, // Used to reserve the first page
	// Reserved memory is just omitted from the list
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
