#include <stdio.h>

#include "loaderapi.h"
#include "percpu.h"
#include "rvmmu.h"
#include "sbi.h"
#include "utils.h"

extern KernelParams kernel_params;

static void handleInterrupt(HartState *hart, uint64_t cause)
{
	hart->scause = cause;
	hart->sepc = hart->pc;

	uint64_t sstatus = hart->sstatus;

	// Set sstatus.spie to sstatus.sie
	if ((sstatus & SSTATUS_SIE) != 0u)
		sstatus |= SSTATUS_SPIE;
	else
		sstatus &= ~SSTATUS_SPIE;

	// Clear mstatus.mie
	sstatus &= ~SSTATUS_SIE;

	// Set sstatus.spp to mode == MODE_USER
	if (hart->mode == HartState::MODE_USER)
		sstatus &= ~SSTATUS_SPP;
	else
		sstatus |= SSTATUS_SPP;

	hart->sstatus = sstatus;
	hart->pc = hart->stvec;
	hart->mode = HartState::MODE_SUPERVISOR;
}

static void handleSRET(HartState *hart)
{
	uint64_t sstatus = hart->sstatus;

	// Set sstatus.sie to sstatus.spie
	if (sstatus & SSTATUS_SPIE)
		sstatus |= SSTATUS_SIE;
	else
		sstatus &= ~SSTATUS_SIE;

	// Go into sstatus.spp mode
	hart->mode = (sstatus & SSTATUS_SPP) ? HartState::MODE_SUPERVISOR : HartState::MODE_USER;

	// Set sstatus.spp to U
	sstatus &= ~SSTATUS_SPP;

	hart->sstatus = sstatus;
	hart->pc = hart->sepc;
}

// Hack
static uint64_t global_time = 0;

static void handlePendingInterrupts(HartState *hart)
{
	if (global_time >= hart->stimecmp)
		hart->sip |= SIP_STIP;

	if((hart->sstatus & SSTATUS_SIE) || hart->mode == HartState::MODE_USER)
	{
		uint64_t ipend = hart->sip & hart->sie;
		if(ipend & SIP_STIP)
			handleInterrupt(hart, HartState::SCAUSE_INTERRUPT_BASE + 5); // IRQ 5
		else if (ipend)
			panic("Unknown interrupt pending");
	}
}

// Perform an instruction fetch of 16 bits at the given addr.
// Returns false on fault.
static __attribute__((warn_unused_result))
bool fetchInstruction(HartState *hart, uint16_t *inst, uint64_t addr)
{
	if (addr & (sizeof(uint16_t) - 1))
		panic("Unaligned instruction fetch");

	// TODO: iTLB, resp. use native load with trap
	auto res = mmu_translate(hart, addr, AccessType::Exec);
	if (!res.pageoff_mask) {
		printf("Instruction fault at %lx\n", addr);
		handleInterrupt(hart, HartState::SCAUSE_INSTR_PAGE_FAULT);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*inst = *phys_to_virt<uint16_t>(phys);
	return true;
}

// TODO: Use native mov with trap here
template <typename T> __attribute__((warn_unused_result))
bool virtRead(HartState *hart, uint64_t addr, T *value)
{
	if (addr & (sizeof(T) - 1))
		panic("Unaligned read");

	auto res = mmu_translate(hart, addr, AccessType::Read);
	if (!res.pageoff_mask) {
		handleInterrupt(hart, HartState::SCAUSE_LOAD_PAGE_FAULT);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*value = *phys_to_virt<T>(phys);
	return true;
}

template <typename T> __attribute__((warn_unused_result))
bool virtWrite(HartState *hart, uint64_t addr, T value)
{
	if (addr & (sizeof(T) - 1))
		panic("Unaligned read");

	auto res = mmu_translate(hart, addr, AccessType::Write);
	if (!res.pageoff_mask) {
		handleInterrupt(hart, HartState::SCAUSE_STORE_PAGE_FAULT);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*phys_to_virt<T>(phys) = value;
	return true;
}

// Dump hart state using printf
void dumpCPUState(HartState *hart)
{
	printf("PC: %016lx (vmlinux: %p, offset: %p)\n",
		hart->pc,
		(void*)(hart->pc - kernel_params.kernel_phys + 0xffffffff80000000),
		(void*)(hart->pc - kernel_params.kernel_phys)
	);

	// Print general regs
	for (int i = 0; i < 32;)
	{
		printf("R%02d: %016lx ", i, hart->regs[i]);
		i++;
		printf("R%02d: %016lx\n", i, hart->regs[i]);
		i++;
	}

	// Print CSRs
	struct { uint64_t *ptr; const char *name; } csrs[] = {
		#define REG(x) { &hart->x, # x }
		REG(sstatus), REG(stvec), REG(sip), REG(sie), REG(sscratch),
		REG(sepc), REG(scause), REG(stval), REG(satp),
		#undef REG
	};

	for (auto csr : csrs)
		printf("%8s: %016lx\n", csr.name, *csr.ptr);

	printf("scounteren: %08x\n", hart->scounteren);
}

static inline uint64_t getReg(HartState *hart, int r)
{
	return hart->regs[r];
}

static inline void setReg(HartState *hart, int r, uint64_t value)
{
	if (r != 0)
		hart->regs[r] = value;
}

static uint64_t getCSR(HartState *hart, uint16_t csr)
{
	// TODO: Permission checks
	switch (csr) {
	case 0x100u:
		return hart->sstatus;
	case 0x104u:
		return hart->sie;
	case 0x140u:
		return hart->sscratch;
	case 0x141u:
		return hart->sepc;
	case 0x142u:
		return hart->scause;
	case 0x143u:
		return hart->stval;
	case 0x180u:
		return hart->satp;
	case 0xc01u:
		return global_time;
	default:
		panic("Unknown CSR read 0x%03x", csr);
	}

	return 0;
}

static void setCSR(HartState *hart, uint16_t csr, uint64_t value)
{
	// TODO: Permission checks
	switch (csr) {
	case 0x100u:
		hart->sstatus = value;
		return;
	case 0x104u:
		hart->sie = value;
		return;
	case 0x105u:
		hart->stvec = value;
		return;
	case 0x106u:
		hart->scounteren = value & 3;
		return;
	case 0x140u:
		hart->sscratch = value;
		return;
	case 0x141u:
		hart->sepc = value;
		return;
	case 0x144u:
		hart->sip = value;
		return;
	case 0x14du:
		hart->stimecmp = value;
		return;
	case 0x180u:
		if ((value >> 60) == 0 || (value >> 60) == 8) // Only bare or Sv39
			hart->satp = value;
		else
			dumpCPUState(hart);
		return;
	default:
		panic("Unknown CSR write 0x%03x", csr);
	}
}

void runThisCPU()
{
	HartState *hart = &getPerCPU()->hart;

	for(;;)
	{
		static uint32_t counter = 0;
		if ((counter++ % 1024) == 0) {
			global_time++;
			handlePendingInterrupts(hart);
		}

		// Fetch 16 bits at a time. Due to IALIGN=16, a 32-bit wide instruction
		// may cross a page boundary and fault.
		uint16_t inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc))
			continue;

		// 16-bit wide compressed instruction?
		if ((inst16 & 0b11) != 0b11)
		{
			const uint16_t inst = inst16;

			if ((inst & 0xE003) == 0xA001) { // c.j
				// This is atrocious.
				uint16_t imm11 = (inst >> 12) & 1,
						 imm4  = (inst >> 11) & 1,
						 imm98 = (inst >>  9) & 3,
						 imm10 = (inst >>  8) & 1,
						 imm6  = (inst >>  7) & 1,
						 imm7  = (inst >>  6) & 1,
						 imm31 = (inst >>  3) & 7,
						 imm5  = (inst >>  2) & 1;

				uint16_t imm = (imm11 << 11) | (imm10 << 10) | (imm98 << 8)
						| (imm7 << 7) | (imm6 << 6) | (imm5 << 5)
						| (imm4 << 4) | (imm31 << 1);

				const int16_t offs = int16_t(imm << 4) >> 4;

				hart->pc += offs;
				continue;
			} else if ((inst & 0xE003) == 0x4001) { // c.li
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >> 2) & 0x1F;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, uint64_t(imm));
			} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'10) { // c.slli
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >> 2) & 0x1F;

				uint16_t imm = (imm5 << 5) | imm40;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, getReg(hart, rd) << imm);
			} else if ((inst & 0xEF83) == 0x6101) { // c.addi16sp (must come before c.lui)
				uint16_t imm9  = (inst >> 12) & 1,
						 imm4  = (inst >>  6) & 1,
						 imm6  = (inst >>  5) & 1,
						 imm87 = (inst >>  3) & 3,
						 imm5  = (inst >>  2) & 1;

				uint16_t uimm = (imm9 << 9) | (imm87 << 7) | (imm6 << 6)
						| (imm5 << 5) | (imm4 << 4);

				int16_t imm = int16_t(uimm << 6) >> 6;
				uint32_t rd = 2;
				setReg(hart, rd, getReg(hart, 2) + imm);
			} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'01) { // c.lui (after c.addi16sp)
				uint16_t imm17   = (inst >> 12) & 1,
						 imm1612 = (inst >>  2) & 0x1F;

				int32_t imm = int32_t(((imm17 << 17) | (imm1612 << 12)) << 14) >> 14;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, int64_t(imm));
			} else if (inst == 0) { // Illegal (before c.addi4spn)
				panic("Illegal 0000 instruction");
			} else if ((inst & 0b111'00000000'000'11) == 0b000'00000000'000'00) { // c.addi4spn (after illegal)
				uint16_t imm54 = (inst >> 11) & 3,
						 imm96 = (inst >>  7) & 0xF,
						 imm2  = (inst >>  6) & 1,
						 imm3  = (inst >>  5) & 1;

				uint16_t uimm = (imm96 << 6) | (imm54 << 4) | (imm3 << 3)
						| (imm2 << 2);

				uint32_t rd = ((inst >> 2) & 7) + 8;
				setReg(hart, rd, getReg(hart, 2) + uimm);
			} else if ((inst & 0b111'0'11111'00000'11) == 0b000'0'00000'00000'01) { // c.nop (before c.addi)
				// nop
			} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'01) { // c.addi (after c.nop)
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >>  2) & 0x1F;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, getReg(hart, rd) + imm);
			} else if ((inst & 0b111'0'00000'00000'11) == 0b001'0'00000'00000'01) { // c.addiw
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >>  2) & 0x1F;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, int64_t(int32_t(getReg(hart, rd) + imm)));
			} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'00'000'00000'01) { // c.srli
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >>  2) & 0x1F;

				uint16_t imm = (imm5 << 5) | imm40;
				uint32_t rd = ((inst >> 7) & 7) + 8;
				setReg(hart, rd, getReg(hart, rd) >> imm);
			} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'01'000'00000'01) { // c.srai
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >>  2) & 0x1F;

				uint16_t imm = (imm5 << 5) | imm40;
				uint32_t rd = ((inst >> 7) & 7) + 8;
				setReg(hart, rd, int64_t(getReg(hart, rd)) >> imm);
			} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'10'000'00000'01) { // c.andi
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >>  2) & 0x1F;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = ((inst >> 7) & 7) + 8;
				setReg(hart, rd, getReg(hart, rd) & uint64_t(imm));
			} else if ((inst & 0b111'000'000'00'000'11) == 0b011'000'000'00'000'00) { // c.ld
				uint16_t imm53 = (inst >> 10) & 7,
						 imm76 = (inst >>  5) & 3;

				uint16_t off = (imm76 << 6) | (imm53 << 3);

				uint32_t rs1 = ((inst >> 7) & 7) + 8,
						 rd  = ((inst >> 2) & 7) + 8;

				uint64_t value;
				if (!virtRead<uint64_t>(hart, getReg(hart, rs1) + off, &value))
					continue;

				setReg(hart, rd, value);
			} else if ((inst & 0b111'000'000'00'000'11) == 0b010'000'000'00'000'00) { // c.lw
				uint16_t imm53 = (inst >> 10) & 7,
						 imm2  = (inst >>  6) & 1,
						 imm6  = (inst >>  5) & 1;

				uint16_t off = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);

				uint32_t rs1 = ((inst >> 7) & 7) + 8,
						 rd  = ((inst >> 2) & 7) + 8;

				int32_t value;
				if (!virtRead<int32_t>(hart, getReg(hart, rs1) + off, &value))
					continue;

				setReg(hart, rd, value);
			} else if ((inst & 0b111'000'000'00'000'11) == 0b110'000'000'00'000'00) { // c.sw
				uint16_t imm53 = (inst >> 10) & 7,
						 imm2  = (inst >>  6) & 1,
						 imm6  = (inst >>  5) & 1;

				uint16_t off = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);

				uint32_t rs1 = ((inst >> 7) & 7) + 8,
						 rs2 = ((inst >> 2) & 7) + 8;

				if (!virtWrite<uint32_t>(hart, getReg(hart, rs1) + off, getReg(hart, rs2)))
					continue;
			} else if ((inst & 0b111'000'000'00'000'11) == 0b111'000'000'00'000'00) { // c.sd
				uint16_t imm53 = (inst >> 10) & 7,
						 imm76 = (inst >>  5) & 3;

				uint16_t off = (imm76 << 6) | (imm53 << 3);

				uint32_t rs1 = ((inst >> 7) & 7) + 8,
						 rs2 = ((inst >> 2) & 7) + 8;

				if (!virtWrite<uint64_t>(hart, getReg(hart, rs1) + off, getReg(hart, rs2)))
					continue;
			} else if ((inst & 0b111'1'00000'11111'11) == 0b100'0'00000'00000'10) { // c.jr (before c.mv)
				uint32_t rs1 = (inst >> 7) & 0x1F;

				if (rs1 == 0)
					panic("Reserved c.???");

				hart->pc = getReg(hart, rs1);
				continue;
			} else if ((inst & 0b111'1'00000'00000'11) == 0b100'0'00000'00000'10) { // c.mv (after c.jr)
				uint32_t rs2 = (inst >> 2) & 0x1F,
						 rd  = (inst >> 7) & 0x1F;

				setReg(hart, rd, getReg(hart, rs2));
			} else if ((inst & 0b111'1'11111'11111'11) == 0b100'1'00000'00000'10) { // c.ebreak
				handleInterrupt(hart, HartState::SCAUSE_EBREAK);
				continue;
			} else if ((inst & 0b111'1'00000'11111'11) == 0b100'1'00000'00000'10) { // c.jalr (after c.ebreak)
				uint32_t rs1 = (inst >> 7u) & 0x1Fu;
				uint64_t retaddr = hart->pc + 2u;
				hart->pc = getReg(hart, rs1);
				setReg(hart, 1, retaddr);
				continue;
			} else if ((inst & 0b111'1'00000'00000'11) == 0b100'1'00000'00000'10) { // c.add (after c.jalr)
				uint32_t rs2 = (inst >> 2) & 0x1F,
						 rd  = (inst >> 7) & 0x1F;

				setReg(hart, rd, getReg(hart, rd) + getReg(hart, rs2));
			} else if ((inst & 0b111'000000'00000'11) == 0b111'000000'00000'10) { // c.sdsp
				uint16_t imm53 = (inst >> 10) & 7,
						 imm86 = (inst >>  7) & 7;

				uint16_t off = (imm86 << 6) | (imm53 << 3);

				uint32_t rs2 = (inst >> 2) & 31;

				if (!virtWrite(hart, getReg(hart, 2) + off, getReg(hart, rs2)))
					continue;
			} else if ((inst & 0b111'000000'00000'11) == 0b110'000000'00000'10) { // c.swsp
				uint16_t imm52 = (inst >>  9) & 0xF,
						 imm76 = (inst >>  7) & 3;

				uint16_t off = (imm76 << 6) | (imm52 << 2);

				uint32_t rs2 = (inst >> 2) & 31;

				if (!virtWrite<uint32_t>(hart, getReg(hart, 2) + off, uint32_t(getReg(hart, rs2))))
					continue;
			} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'10) { // c.ldsp
				uint16_t imm5  = (inst >> 12) & 1,
						 imm43 = (inst >>  5) & 3,
						 imm86 = (inst >>  2) & 7;

				uint16_t off = (imm86 << 6) | (imm5 << 5) | (imm43 << 3);

				uint32_t rd = (inst >> 7) & 31;

				if (rd == 0)
					panic("Reserved instruction");

				uint64_t value;
				if (!virtRead(hart, getReg(hart, 2) + off, &value))
					continue;

				setReg(hart, rd, value);
			} else if ((inst & 0b111'0'00000'00000'11) == 0b010'0'00000'00000'10) { // c.lwsp
				uint16_t imm5  = (inst >> 12) & 1,
						 imm42 = (inst >>  4) & 7,
						 imm76 = (inst >>  2) & 3;

				uint16_t off = (imm76 << 6) | (imm5 << 5) | (imm42 << 2);

				uint32_t rd = (inst >> 7) & 31;

				if (rd == 0)
					panic("Reserved instruction");

				int32_t value;
				if (!virtRead(hart, getReg(hart, 2) + off, &value))
					continue;

				setReg(hart, rd, value);
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'00'000'01) { // c.sub
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, getReg(hart, rd) - getReg(hart, rs2));
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'01'000'01) { // c.xor
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, getReg(hart, rd) ^ getReg(hart, rs2));
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'10'000'01) { // c.or
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, getReg(hart, rd) | getReg(hart, rs2));
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'11'000'01) { // c.and
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, getReg(hart, rd) & getReg(hart, rs2));
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'1'11'000'00'000'01) { // c.subw
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, int64_t(int32_t(getReg(hart, rd)) - int32_t(getReg(hart, rs2))));
			} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'1'11'000'01'000'01) { // c.addw
				uint32_t rs2 = ((inst >> 2) & 7) + 8,
						 rd  = ((inst >> 7) & 7) + 8;

				setReg(hart, rd, int64_t(int32_t(getReg(hart, rd)) + int32_t(getReg(hart, rs2))));
			} else if ((inst & 0b111'000'000'00000'11) == 0b110'000'000'00000'01) { // c.beqz
				uint16_t imm8  = (inst >> 12) & 1,
						 imm43 = (inst >> 10) & 3,
						 imm76 = (inst >>  5) & 3,
						 imm21 = (inst >>  3) & 3,
						 imm5  = (inst >>  2) & 1;

				uint16_t uimm = (imm8 << 8) | (imm76 << 6) | (imm5 << 5) | (imm43 << 3)
								| (imm21 << 1);

				int16_t imm = int16_t(uimm << 7) >> 7;

				uint32_t rs1 = ((inst >> 7) & 7) + 8;

				if (getReg(hart, rs1) == 0) {
					hart->pc += imm;
					continue;
				}
			} else if ((inst & 0b111'000'000'00000'11) == 0b111'000'000'00000'01) { // c.bnez
				uint16_t imm8  = (inst >> 12) & 1,
						 imm43 = (inst >> 10) & 3,
						 imm76 = (inst >>  5) & 3,
						 imm21 = (inst >>  3) & 3,
						 imm5  = (inst >>  2) & 1;

				uint16_t uimm = (imm8 << 8) | (imm76 << 6) | (imm5 << 5) | (imm43 << 3)
								| (imm21 << 1);

				int16_t imm = int16_t(uimm << 7) >> 7;

				uint32_t rs1 = ((inst >> 7) & 7) + 8;

				if (getReg(hart, rs1) != 0) {
					hart->pc += imm;
					continue;
				}
			} else {
				panic("Unknown instruction %04x", inst);
			}

			hart->pc += 2;
			continue;
		}

		// (at least) 32-bit wide instruction. Fetch the remaining 16 bits.
		uint32_t inst = inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc + 2))
			continue;

		inst |= inst16 << 16;

		uint32_t opc = inst & 0x7Fu;
		switch(opc) {
		case 0x0fu: // fences
			break;
		case 0x03u: // load
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int32_t imm = int32_t(inst) >> 20u;

			uint64_t addr = getReg(hart, rs1) + imm;
			switch (funct3)
			{
				case 0u: { // lb
					int8_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, int64_t(val));
					break;
				}
				case 1u: { // lh
					int16_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, int64_t(val));
					break;
				}
				case 2u: { // lw
					int32_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, int64_t(val));
					break;
				}
				case 3u: { // ld
					int64_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 4u: { // lbu
					uint8_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, uint64_t(val));
					break;
				}
				case 5u: { // lhu
					uint16_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, uint64_t(val));
					break;
				}
				case 6u: { // lwu
					uint32_t val;
					if (!virtRead(hart, addr, &val))
						continue;

					setReg(hart, rd, uint64_t(val));
					break;
				}
				default:
					panic("Unknown load instruction");
			}
			break;
		}
		case 0x13u: // integer immediate
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int64_t imm = int32_t(inst) >> 20u;
			uint64_t rawimm = inst >> 20u;
			switch (funct3)
			{
				case 0x0u: // addi
					setReg(hart, rd, int64_t(getReg(hart, rs1)) + imm);
					break;
				case 0x1u: // slli
					if ((rawimm >> 6u) == 0) // slli
						setReg(hart, rd, getReg(hart, rs1) << (rawimm & 63u));
					else
						panic("Shift not supported");

					break;
				case 0x2u: // slti
					setReg(hart, rd, (int64_t(getReg(hart, rs1)) < imm) ? 1u : 0u);
					break;
				case 0x3u: // sltiu
					setReg(hart, rd, (getReg(hart, rs1) < uint64_t(imm)) ? 1u : 0u);
					break;
				case 0x4u: // xori
					setReg(hart, rd, getReg(hart, rs1) ^ imm);
					break;
				case 0x5u: // sr(l,a)i
					if ((rawimm >> 6u) == 0) // srli
						setReg(hart, rd, getReg(hart, rs1) >> (rawimm & 63u));
					else if ((rawimm >> 6u) == 0x10u) // srai
						setReg(hart, rd, int64_t(getReg(hart, rs1)) >> (rawimm & 63u));
					else
						panic("Shift not supported");

					break;
				case 0x6u: // ori
					setReg(hart, rd, getReg(hart, rs1) | imm);
					break;
				case 0x7u: // andi
					setReg(hart, rd, getReg(hart, rs1) & imm);
					break;
				default:
					panic("Unsupported instruction");
			}
			break;
		}
		case 0x17u: // auipc
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			setReg(hart, rd, hart->pc + int32_t(inst & 0xFFFFF000u));
			break;
		}
		case 0x1Bu: // integer immediate (RV64I)
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int64_t imm = int32_t(inst) >> 20u;
			uint64_t rawimm = inst >> 20u;
			switch (funct3)
			{
				case 0x0u: // addiw
					setReg(hart, rd, int64_t(int32_t(int64_t(getReg(hart, rs1)) + imm)));
					break;
				case 0x1u: // slliw
					if ((rawimm >> 5u) == 0) // slliw
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1) << (rawimm & 31u))));
					else
						panic("Shift not supported");

					break;
				case 0x5u: // sr(l,a)iw
					if ((rawimm >> 5u) == 0) // srliw
						setReg(hart, rd, int32_t(uint32_t(getReg(hart, rs1)) >> (rawimm & 31u)));
					else if ((rawimm >> 6u) == 0x10u) // sraiw
						setReg(hart, rd, int32_t(uint32_t(getReg(hart, rs1))) >> (rawimm & 31u));
					else
						panic("Shift not supported");

					break;
				default:
					panic("Unsupported instruction");
			}
			break;
		}
		case 0x23u: // store
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			int32_t imm = ((int32_t(inst) >> 25u) << 5u) | ((inst >> 7u) & 0x1Fu);
			uint64_t addr = getReg(hart, rs1) + imm;
			switch (funct3)
			{
			case 0u: // sb
				if (!virtWrite<uint8_t>(hart, addr, getReg(hart, rs2)))
					continue;
				break;
			case 1u: // sh
				if (!virtWrite<uint16_t>(hart, addr, getReg(hart, rs2)))
					continue;
				break;
			case 2u: // sw
				if (!virtWrite<uint32_t>(hart, addr, getReg(hart, rs2)))
					continue;
				break;
			case 3u: // sd
				if (!virtWrite<uint64_t>(hart, addr, getReg(hart, rs2)))
					continue;
				break;
			default:
				panic("Unknown store");
			}
			break;
		}
		case 0x2fu: // atomic extension
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			uint32_t funct7 = inst >> 25u;

			// ignore aq and rl bits
			funct7 &= ~3u;

			switch((funct3 << 8u) | funct7)
			{
				case 0x200u: // amoadd.w
				{
					uint64_t addr = getReg(hart, rs1);
					uint32_t val;
					if (!virtRead<uint32_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint32_t>(hart, addr, val + uint32_t(getReg(hart, rs2))))
						continue;

					setReg(hart, rd, int64_t(int32_t(val)));
					break;
				}
				case 0x204u: // amoswap.w
				{
					uint64_t addr = getReg(hart, rs1);
					uint32_t val;
					if (!virtRead<uint32_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint32_t>(hart, addr, getReg(hart, rs2)))
						continue;

					setReg(hart, rd, int64_t(int32_t(val)));
					break;
				}
				case 0x208u: // lr.w
				{
					if (rs2 != 0u)
						panic("lr with non-zero");

					uint64_t addr = getReg(hart, rs1);
					uint32_t val;
					if (!virtRead<uint32_t>(hart, addr, &val))
						continue;

					setReg(hart, rd, int32_t(val));
					break;
				}
				case 0x20cu: // sc.w
				{
					uint64_t addr = getReg(hart, rs1);
					if (!virtWrite<uint32_t>(hart, addr, getReg(hart, rs2)))
						continue;

					setReg(hart, rd, 0u);
					break;
				}
				case 0x300u: // amoadd.d
				{
					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint64_t>(hart, addr, val + getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x304u: // amoswap.d
				{
					// TODO: Is this correct? Several docs disagree...
					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint64_t>(hart, addr, getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x308u: // lr.d
				{
					if (rs2 != 0u)
						panic("lr with non-zero");

					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x30cu: // sc.d
				{
					uint64_t addr = getReg(hart, rs1);
					if (!virtWrite<uint64_t>(hart, addr, getReg(hart, rs2)))
						continue;

					setReg(hart, rd, 0u);
					break;
				}
				case 0x310u: // amoxor.d
				{
					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint64_t>(hart, addr, val ^ getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x320u: // amoor.d
				{
					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint64_t>(hart, addr, val | getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x220u: // amoor.w
				{
					uint64_t addr = getReg(hart, rs1);
					uint32_t val;
					if (!virtRead<uint32_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint32_t>(hart, addr, val | getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				case 0x330u: // amoand.d
				{
					uint64_t addr = getReg(hart, rs1);
					uint64_t val;
					if (!virtRead<uint64_t>(hart, addr, &val))
						continue;

					if (!virtWrite<uint64_t>(hart, addr, val & getReg(hart, rs2)))
						continue;

					setReg(hart, rd, val);
					break;
				}
				default:
					panic("Unimplemented atomic instruction");
			}
			break;
		}
		case 0x33u: // integer register
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			uint32_t funct7 = inst >> 25u;

			switch((funct3 << 8u) | funct7)
			{
				case 0x000u: // add
					setReg(hart, rd, getReg(hart, rs1) + getReg(hart, rs2));
					break;
				case 0x001u: // mul
					setReg(hart, rd, getReg(hart, rs1) * getReg(hart, rs2));
					break;
				case 0x020u: // sub
					setReg(hart, rd, getReg(hart, rs1) - getReg(hart, rs2));
					break;
				case 0x100u: // sll
					setReg(hart, rd, getReg(hart, rs1) << (getReg(hart, rs2) & 63u));
					break;
				case 0x101u: // mulh
					setReg(hart, rd, (__int128_t(int64_t(getReg(hart, rs1))) * __int128_t(int64_t(getReg(hart, rs2)))) >> 64);
					break;
				case 0x200u: // slt
					setReg(hart, rd, int64_t(getReg(hart, rs1)) < int64_t(getReg(hart, rs2)) ? 1u : 0u);
					break;
				case 0x201u: // mulhsu
					setReg(hart, rd, (__int128_t(int64_t(getReg(hart, rs1))) * __uint128_t(getReg(hart, rs2))) >> 64);
					break;
				case 0x300u: // sltu
					setReg(hart, rd, (getReg(hart, rs1) < getReg(hart, rs2)) ? 1u : 0u);
					break;
				case 0x301u: // mulhu
					setReg(hart, rd, (__uint128_t(getReg(hart, rs1)) * __uint128_t(getReg(hart, rs2))) >> 64);
					break;
				case 0x400u: // xor
					setReg(hart, rd, getReg(hart, rs1) ^ getReg(hart, rs2));
					break;
				case 0x401u: // div
					if (getReg(hart, rs2) == 0)
						setReg(hart, rd, ~uint64_t(0));
					else if (int64_t(getReg(hart, rs1)) == INT64_MIN && int64_t(getReg(hart, rs2)) == -1)
						setReg(hart, rd, uint64_t(INT64_MIN));
					else
						setReg(hart, rd, int64_t(getReg(hart, rs1)) / int64_t(getReg(hart, rs2)));

					break;
				case 0x500u: // srl
					setReg(hart, rd, getReg(hart, rs1) >> (getReg(hart, rs2) & 63u));
					break;
				case 0x501u: // divu
					if (getReg(hart, rs2) == 0)
						setReg(hart, rd, ~uint64_t(0));
					else
						setReg(hart, rd, getReg(hart, rs1) / getReg(hart, rs2));

					break;
				case 0x520u: // sra
					setReg(hart, rd, int64_t(getReg(hart, rs1)) >> (getReg(hart, rs2) & 63u));
					break;
				case 0x600u: // or
					setReg(hart, rd, getReg(hart, rs1) | getReg(hart, rs2));
					break;
				case 0x601u: // rem
					// TODO: Signedness correct?
					if (getReg(hart, rs2) == 0)
						setReg(hart, rd, getReg(hart, rs1));
					else if (int64_t(getReg(hart, rs1)) == INT64_MIN && int64_t(getReg(hart, rs2)) == -1)
						setReg(hart, rd, 0);
					else
						setReg(hart, rd, int64_t(getReg(hart, rs1)) % int64_t(getReg(hart, rs2)));
					break;
				case 0x700u: // and
					setReg(hart, rd, getReg(hart, rs1) & getReg(hart, rs2));
					break;
				case 0x701u: // remu
					if (getReg(hart, rs2) == 0)
						setReg(hart, rd, getReg(hart, rs1));
					else
						setReg(hart, rd, getReg(hart, rs1) % getReg(hart, rs2));
					break;
				default:
					panic("Unknown reg-reg instruction");
			}
			break;
		}
		case 0x37u: // lui
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			setReg(hart, rd, int64_t(int32_t(inst & 0xFFFFF000u)));
			break;
		}
		case 0x3Bu: // integer register (RV64)
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			uint32_t funct7 = inst >> 25u;

			switch((funct3 << 8u) | funct7)
			{
				case 0x000u: // addw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) + int32_t(getReg(hart, rs2))));
					break;
				case 0x001u: // mulw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) * int32_t(getReg(hart, rs2))));
					break;
				case 0x020u: // subw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) - int32_t(getReg(hart, rs2))));
					break;
				case 0x100u: // sllw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) << (getReg(hart, rs2) & 31u)));
					break;
				case 0x401u: // divw
					if (uint32_t(getReg(hart, rs2)) == 0)
						setReg(hart, rd, ~uint64_t(0));
					else if (int32_t(getReg(hart, rs1)) == INT32_MIN && int32_t(getReg(hart, rs2)) == -1)
						setReg(hart, rd, int64_t(INT32_MIN));
					else
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) / int32_t(getReg(hart, rs2))));
					break;
				case 0x500u: // srlw
					setReg(hart, rd, int64_t(int32_t(uint32_t(getReg(hart, rs1)) >> (getReg(hart, rs2) & 31u))));
					break;
				case 0x501u: // divuw
					if (uint32_t(getReg(hart, rs2)) == 0)
						setReg(hart, rd, ~uint64_t(0));
					else
						setReg(hart, rd, int64_t(int32_t(uint32_t(getReg(hart, rs1)) / uint32_t(getReg(hart, rs2)))));
					break;
				case 0x520u: // sraw
					setReg(hart, rd, int64_t(int32_t(uint32_t(getReg(hart, rs1))) >> (getReg(hart, rs2) & 31u)));
					break;
				case 0x601u: // remw
					if (uint32_t(getReg(hart, rs2)) == 0)
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1))));
					else if (int32_t(getReg(hart, rs1)) == INT32_MIN && int32_t(getReg(hart, rs2)) == -1)
						setReg(hart, rd, 0);
					else
						setReg(hart, rd, int64_t(int32_t(int32_t(getReg(hart, rs1)) % int32_t(getReg(hart, rs2)))));
					break;
				case 0x701u: // remuw
					if (uint32_t(getReg(hart, rs2)) == 0)
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1))));
					else
						setReg(hart, rd, int64_t(int32_t(uint32_t(getReg(hart, rs1)) % uint32_t(getReg(hart, rs2)))));
					break;
				default:
					panic("Unknown 32-bit reg-reg instruction");
			}
			break;
		}
		case 0x63u: // branch
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			int64_t rs1vals = int64_t(getReg(hart, rs1));
			int64_t rs2vals = int64_t(getReg(hart, rs2));
			uint64_t rs1valu = getReg(hart, rs1);
			uint64_t rs2valu = getReg(hart, rs2);
			uint32_t imm12 = inst >> 31u;
			uint32_t imm105 = (inst >> 25u) & 0x3fu;
			uint32_t imm41 = (inst >> 8u) & 0xfu;
			uint32_t imm11 = (inst >> 7u) & 0x1u;
			int32_t imm = int32_t(((imm12 << 12u) | (imm11 << 11u) | (imm105 << 5u) | (imm41 << 1u)) << 19u) >> 19u;
			bool take = false;
			switch (funct3)
			{
				case 0u: // beq
					take = rs1valu == rs2valu;
					break;
				case 1u: // bne
					take = rs1valu != rs2valu;
					break;
				case 4u: // blt
					take = rs1vals < rs2vals;
					break;
				case 5u: // bge
					take = rs1vals >= rs2vals;
					break;
				case 6u: // bltu
					take = rs1valu < rs2valu;
					break;
				case 7u: // bgeu
					take = rs1valu >= rs2valu;
					break;
				default:
					panic("Unsupported branch");
			}
			if (take) {
				hart->pc += imm;
				continue;
			}
			break;
		}
		case 0x67u: // jalr
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int32_t imm = int32_t(inst) >> 20u;

			uint64_t retaddr = hart->pc + 4u;
			hart->pc = getReg(hart, rs1) + imm;
			setReg(hart, rd, retaddr);
			continue;
		}
		case 0x6fu: // jal
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			uint32_t imm20 = inst >> 31u;
			uint32_t imm101 = (inst >> 21u) & 0x3FFu;
			uint32_t imm11 = (inst >> 20u) & 1u;
			uint32_t imm1912 = (inst >> 12u) & 0xFFu;
			int32_t imm = int32_t(((imm20 << 20u) | (imm1912 << 12u) | (imm11 << 11u) | (imm101 << 1u)) << 11u) >> 11u;
			setReg(hart, rd, hart->pc + 4u);
			hart->pc += imm;
			continue;
		}
		case 0x73u: // SYSTEM
		{
			uint32_t funct3 = (inst >> 12u) & 0x7u;
			switch(funct3)
			{
			case 0u: // Misc stuff
			{
				if (inst == 0x00000073u) { // ecall
					if (hart->mode == HartState::MODE_SUPERVISOR)
						handleSBICall(hart);
					else {
						handleInterrupt(hart, HartState::SCAUSE_ECALL_UMODE);
						continue;
					}
				} else if (inst == 0x00100073u) {
					panic("ebreak");
				} else if ((inst & 0b1111111'00000'00000'111'11111'1111111) == 0b0001001'00000'00000'000'00000'1110011) {
					//printf("Doing some fencing\n");
				} else if (inst == 0x10200073) {
					handleSRET(hart);
					continue;
				} else
					panic("Unsupported misc instruction");

				break;
			}
			case 1u: // CSRRW
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				unsigned int rs1 = (inst >> 15u) & 31u;
				uint64_t rs1val = getReg(hart, rs1);

				if (rd != 0u) // No getCSR side effect if rd is zero
					setReg(hart, rd, getCSR(hart, csr));

				setCSR(hart, csr, rs1val);
				break;
			}
			case 2u: // CSRRS
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				unsigned int rs1 = (inst >> 15u) & 31u;
				uint64_t csrval = getCSR(hart, csr);
				uint64_t rs1val = getReg(hart, rs1);
				setReg(hart, rd, getCSR(hart, csr));

				if (rs1 != 0) // No setCSR side effect if rs1 is zero
					setCSR(hart, csr, csrval | rs1val);

				break;
			}
			case 3u: // CSRRC
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				unsigned int rs1 = (inst >> 15u) & 31u;
				uint64_t csrval = getCSR(hart, csr);
				uint64_t rs1val = getReg(hart, rs1);
				setReg(hart, rd, getCSR(hart, csr));

				if (rs1 != 0) // No setCSR side effect if rs1 is zero
					setCSR(hart, csr, csrval & ~rs1val);

				break;
			}
			case 5u: // CSRRWI
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				if (rd != 0u) // No getCSR side effect if rd is zero
					setReg(hart, rd, getCSR(hart, csr));

				uint64_t imm = (inst >> 15u) & 31u;
				setCSR(hart, csr, imm);
				break;
			}
			case 6u: // CSRRSI
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				uint64_t imm = (inst >> 15u) & 31u;
				uint64_t csrval = getCSR(hart, csr);
				setReg(hart, rd, csrval);
				setCSR(hart, csr, csrval | imm);
				break;
			}
			case 7u: // CSRRCI
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				uint64_t imm = (inst >> 15u) & 31u;
				uint64_t csrval = getCSR(hart, csr);
				setReg(hart, rd, csrval);
				setCSR(hart, csr, csrval & ~imm);
				break;
			}
			default:
				panic("Unknown SYSTEM instruction");
			}

			// Immediately check for interrupts
			hart->pc += 4;
			continue;
		}
		default:
			panic("Unknown instruction %08x", inst);
		}

		hart->pc += 4;
	}
}
