#include <stdio.h>
#include <string.h>

#include "percpu.h"
#include "mem.h"
#include "smp.h"
#include "uacpi/acpi.h"
#include "uacpi/tables.h"
#include "utils.h"
#include "x86interrupts.h"

static uint8_t cpusFound = 0;

static struct {
	uint8_t cpuID, lapicID;
} lapics[MAX_CPUS];

/* Getting secondary cores to run kernel code is not easy on x86.
 * When they receive the startup command, they jump to (vector & 0xFF) << 12
 * in real mode, which means the startup code must be in low memory (< 1MiB)
 * and set up long mode (GDT, paging, stack) before jumping to the kernel.
 *
 * The physical memory layout of lowmem is not guaranteed, so the trampoline
 * tries to be as position-independent as possible. There's no RIP-relative
 * addressing in real mode though, so that has to be done by hand.
 * ESI is set up to contain the absolute address of smp_trampoline,
 * so all memory access can be done ESI-relative.
 * The literal for setting ESI as well as some data values are relocated
 * by the setup code before running it. */
asm(R"asm(
.code16
smp_trampoline:
cli
cld # Clear direction flag - required by the C ABI

# The literal is overwritten with the abs. address of smp_trampoline
smp_trampoline_reloc_esi:
mov $0xdeadbeef, %esi

# Enable PAE and PGE
mov %cr4, %eax
or $0b10100000, %eax
mov %eax, %cr4

# Set CR3 to the PML4
mov (smp_trampoline_pml4 - smp_trampoline)(%esi), %eax
mov %eax, %cr3

# Set LM flag in EFER
mov $0xC0000080, %ecx
rdmsr
or $0x00000100, %eax
wrmsr

# Enable paging and protected mode
mov %cr0, %eax
or $0x80000001, %eax
mov %eax, %cr0

# Load the GDT
lgdtl (smp_trampoline_gdtp - smp_trampoline)(%esi)

# Far jump to $0x08:smp_trampoline_64, but this is only
# possible with an indirect instruction...
ljmpl *(smp_trampoline_64_addr - smp_trampoline)(%esi)
smp_trampoline_64_addr:
.long (smp_trampoline_64 - smp_trampoline)
.word 0x8

.code64
smp_trampoline_64:
# Finally, long mode!

# Get the CPU's number into %edi
xor %eax, %eax
movabs $cpusOnline, %rbx
movl (%rbx), %eax
create_cpu_num:
lea 1(%eax), %edi
lock cmpxchgl %edi, (%rbx)
jnz create_cpu_num
# CPU numbers start at 0
dec %edi

# Set up the stack pointer
# TODO: KERNEL_STACK_CPU_OFFSET
mov $(2 * 1 * 1024 * 1024), %rax
imul %rdi
# TODO: KERNEL_STACK_SIZE
add $(1 * 1024 * 1024 - 8), %rax
# TODO: KERNEL_STACK_LOW
movabs $(0xFFFF800000000000UL), %rbx
add %rbx, %rax
mov %rax, %rsp

# Jump to secondaryEntry. $edi is already set above.
movabs $secondaryEntry, %rax
jmp *%rax

.balign 4
smp_trampoline_pml4:
.long 0
smp_trampoline_gdt:
.quad 0x0000000000000000
.quad (0b10101111 << 48) | (0b10011011 << 40) | 0xFFFF // 64-bit kernel code segment
.quad (0b10001111 << 48) | (0b10010011 << 40) | 0xFFFF // 64-bit kernel data segment

.word 0
smp_trampoline_gdtp:
.word 3*8-1
smp_trampoline_reloc_gdtp:
.long smp_trampoline_gdt - smp_trampoline

smp_trampoline_end:
)asm");

// Variables provided and referenced by the assembly code above.
extern "C" {
	// Range of the trampoline that needs to be copied.
	extern uint8_t smp_trampoline[], smp_trampoline_end;
	// Locations that need relocations applied:
	extern uint8_t smp_trampoline_reloc_esi[], smp_trampoline_64_addr[],
	               smp_trampoline_reloc_gdtp[];
	// Value of %cr3 to use.
	extern uint32_t smp_trampoline_pml4;
	// Incremented by each CPU that comes online (incl. CPU0, the BSP)
	volatile uint32_t cpusOnline = 0;
	// Called once long mode and stack are set up.
	void secondaryEntry(unsigned int cpuNum);
}

void secondaryEntry(unsigned int cpuNum)
{
	printf("CPU %u is here\n", cpuNum);
	asm volatile("hlt");
}

void setupSMP()
{
	// Iterate the MADT to find all LAPICS of online-able CPUs.
	uacpi_table tbl;
	if (uacpi_table_find_by_signature("APIC", &tbl) != UACPI_STATUS_OK)
		panic("MADT not found");

	acpi_madt *madt = reinterpret_cast<acpi_madt*>(tbl.ptr);

	auto cb = [] (uacpi_handle user, acpi_entry_hdr *hdr) {
		(void) user;
		if (hdr->type == ACPI_MADT_ENTRY_TYPE_LAPIC) {
			auto lapic_tbl = reinterpret_cast<acpi_madt_lapic*>(hdr);
			if (lapic_tbl->flags & (ACPI_PIC_ENABLED | ACPI_PIC_ONLINE_CAPABLE)) {
				if (cpusFound >= MAX_CPUS) {
					printf("More CPUs than MAX_CPUS (%d), ignoring additional ones.\n", MAX_CPUS);
					return UACPI_ITERATION_DECISION_BREAK;
				}

				lapics[cpusFound++] = {
				    .cpuID = lapic_tbl->uid,
				    .lapicID = lapic_tbl->id,
				};
			}
		}

		return UACPI_ITERATION_DECISION_CONTINUE;
	};

	uacpi_for_each_subtable(&madt->hdr, sizeof(*madt), cb, NULL);

	printf("Found %d CPU%s\n", cpusFound, cpusFound > 1 ? "s" : "");

	uintptr_t pml4;
	asm volatile("mov %%cr3, %[pml4]" : [pml4] "=r" (pml4));
	if (pml4 >> 32)
		panic("PML4 not in 32bit address space");

	smp_trampoline_pml4 = uint32_t(pml4);

	size_t smp_trampoline_size = uintptr_t(&smp_trampoline_end) - uintptr_t(smp_trampoline);

	if (smp_trampoline_size > 4096)
		panic("Trampoline too big!");

	PhysAddr trampoline = physMemMgr.allocate(4096, MemRegionFree);
	if (trampoline & 0xFFF)
		panic("Trampoline not page aligned");

	if (trampoline > 0xFF000)
		panic("Trampoline not in lowmem");

	memcpy(phys_to_virt<uint8_t>(trampoline), smp_trampoline, smp_trampoline_size);

	// Perform manual relocations in the trampoline code
	// Change the literal of mov $0xdeadbeef, %esi
	memcpy(phys_to_virt<uint8_t>(trampoline) + uintptr_t(smp_trampoline_reloc_esi) - uintptr_t(smp_trampoline) + 2, &trampoline, 4);

	// Add the trampoline address to some relocations
	auto relocAdd = [=](PhysAddr longRelocAddr) {
		uint32_t relocValue;
		memcpy(&relocValue, phys_to_virt<uint8_t>(longRelocAddr), sizeof(relocValue));
		relocValue += trampoline;
		memcpy(phys_to_virt<uint8_t>(longRelocAddr), &relocValue, sizeof(relocValue));
	};
	relocAdd(trampoline + uintptr_t(smp_trampoline_64_addr) - uintptr_t(smp_trampoline));
	relocAdd(trampoline + uintptr_t(smp_trampoline_reloc_gdtp) - uintptr_t(smp_trampoline));

	// The BSP is CPU 0
	cpusOnline = 1;

	for (int i = 0; i < cpusFound; ++i) {
		printf("LAPIC %x\n", lapics[i].lapicID);
		// TODO: Get LAPIC ID of BSP
		if (lapics[i].lapicID == 0) {
			continue;
		}

		// Send INIT IPI
		lapicWrite(0x310, uint32_t(lapics[i].lapicID) << 24);
		lapicWrite(0x300, (1 << 14) | (0b101 << 8));

		// Wait until sent
		while (lapicRead(0x310) & (1 << 14)) asm volatile("pause");
		// TODO: At least 10ms?

		// Sent S(tartup)IPI
		lapicWrite(0x310, uint32_t(lapics[i].lapicID) << 24);
		lapicWrite(0x300, (1 << 14) | (0b110 << 8) | (trampoline >> 12));

		// Wait until sent
		while (lapicRead(0x310) & (1 << 14)) asm volatile("pause");
	}

	printf("Waiting for secondary CPUs to come online...\n");
	while (cpusOnline < cpusFound) asm volatile("pause");
}
