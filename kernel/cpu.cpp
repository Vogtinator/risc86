#include <stdio.h>

#include "percpu.h"
#include "rvmmu.h"
#include "utils.h"

// Perform an instruction fetch of 16 bits at the given addr.
// Returns false on fault.
static bool fetchInstruction(HartState *hart, uint16_t *inst, uint64_t addr)
{
	// TODO: iTLB, resp. use native load with trap
	auto res = mmu_translate(hart, addr);
	if(!res.pageoff_mask)
		return false; // TODO: Indicate instruction fault

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*inst = *phys_to_virt<uint16_t>(phys);
	return true;
}

// Dump hart state using printf
static void dumpCPUState(HartState *hart)
{
	printf("PC: %016lx\n", hart->pc);
	for(int i = 0; i < 32;)
	{
		printf("R%02d: %016lx ", i, hart->regs[i]);
		i++;
		printf("R%02d: %016lx\n", i, hart->regs[i]);
		i++;
	}
}

void runThisCPU()
{
	HartState *hart = &getPerCPU()->hart;

	for(;;)
	{
		dumpCPUState(hart);

		// Fetch 16 bits at a time. Due to IALIGN=16, a 32-bit wide instruction
		// may cross a page boundary and fault.
		uint16_t inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc))
			panic("Instruction fault");

		// 16-bit wide compressed instruction?
		if ((inst16 & 0b11) != 0b11)
		{
			printf("Instruction: %04x\n", inst16);
			hart->pc += 2;
			continue;
		}

		// (at least) 32-bit wide instruction. Fetch the remaining 16 bits.
		uint32_t inst32 = inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc + 2))
			panic("Instruction fault");

		inst32 |= inst16 << 16;
		printf("Instruction: %04x\n", inst32);
		hart->pc += 4;
	}
}
