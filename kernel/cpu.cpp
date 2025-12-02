#include <stdio.h>

#include "percpu.h"
#include "rvmmu.h"
#include "utils.h"

// Perform an instruction fetch of 16 bits at the given addr.
// Returns false on fault.
static bool fetchInstruction(HartState *hart, uint16_t *inst, uint64_t addr)
{
	if (addr & (sizeof(uint16_t) - 1))
		panic("Unaligned instruction fetch");

	// TODO: iTLB, resp. use native load with trap
	auto res = mmu_translate(hart, addr, AccessType::Exec);
	if(!res.pageoff_mask)
		return false; // TODO: Indicate instruction fault

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*inst = *phys_to_virt<uint16_t>(phys);
	return true;
}

// TODO: Use native mov with trap here
template <typename T> bool virtRead(HartState *hart, uint64_t addr, T *value)
{
	if (addr & (sizeof(T) - 1))
		panic("Unaligned read");

	auto res = mmu_translate(hart, addr, AccessType::Read);
	if(!res.pageoff_mask)
		return false; // TODO: Indicate fault

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*value = *phys_to_virt<T>(phys);
	return true;
}

template <typename T> bool virtWrite(HartState *hart, uint64_t addr, T value)
{
	if (addr & (sizeof(T) - 1))
		panic("Unaligned read");

	auto res = mmu_translate(hart, addr, AccessType::Write);
	if(!res.pageoff_mask)
		return false; // TODO: Indicate fault

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*phys_to_virt<T>(phys) = value;
	return true;
}

// Dump hart state using printf
void dumpCPUState(HartState *hart)
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
	case 0x106u:
		hart->scounteren = value & 3;
		return;
	case 0x144u:
		hart->sip = value;
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
		//dumpCPUState(hart);

		// Fetch 16 bits at a time. Due to IALIGN=16, a 32-bit wide instruction
		// may cross a page boundary and fault.
		uint16_t inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc))
			panic("Instruction fault");

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
			} else if ((inst & 0xEF83) == 0x6101) { // c.addi16sp (must come before c.lui)
				uint16_t imm9  = (inst >> 12) & 1,
						 imm4  = (inst >>  6) & 1,
						 imm6  = (inst >>  5) & 1,
						 imm87 = (inst >>  3) & 3,
						 imm5  = (inst >>  2) & 1;

				uint16_t uimm = (imm9 << 9) | (imm87 << 7) | (imm6 << 6)
						| (imm5 << 5) | (imm4 << 4);

				int16_t imm = int16_t(uimm << 5) >> 5;
				uint32_t rd = 2;
				setReg(hart, rd, getReg(hart, 2) + imm);
			} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'01) { // c.lui (after c.addi16sp)
				uint16_t imm17   = (inst >> 12) & 1,
						 imm1612 = (inst >>  2) & 0x1F;

				int32_t imm = int32_t(((imm17 << 17) | (imm1612 << 12)) << 14) >> 14;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, uint64_t(imm));
			} else if ((inst & 0b111'0'11111'00000'11) == 0b000'0'00000'00000'01) { // c.nop (before c.addi)
				// nop
			} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'01) { // c.addi (after c.nop)
				uint16_t imm5  = (inst >> 12) & 1,
						 imm40 = (inst >> 2) & 0x1F;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, getReg(hart, rd) + imm);
			} else {
				panic("Unknown instruction");
			}

			hart->pc += 2;
			continue;
		}

		// (at least) 32-bit wide instruction. Fetch the remaining 16 bits.
		uint32_t inst = inst16;
		if (!fetchInstruction(hart, &inst16, hart->pc + 2))
			panic("Instruction fault");

		inst |= inst16 << 16;

		uint32_t opc = inst & 0x7Fu;
		switch(opc) {
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
					setReg(hart, rd, (getReg(hart, rs1) < rawimm) ? 1u : 0u);
					break;
				case 0x4u: // xori
					setReg(hart, rd, getReg(hart, rs1) ^ rawimm);
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
					setReg(hart, rd, getReg(hart, rs1) | rawimm);
					break;
				case 0x7u: // andi
					setReg(hart, rd, getReg(hart, rs1) & rawimm);
					break;
				default:
					panic("Unsupported instruction");
			}
			break;
		}
		case 0x17u: // auipc
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			setReg(hart, rd, hart->pc + (inst & 0xFFFFF000u));
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
				case 0x5u: // sr(l,a)i
					if ((rawimm >> 5u) == 0) // srliw
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1) >> (rawimm & 31u))));
					else if ((rawimm >> 6u) == 0x10u) // srai
						setReg(hart, rd, int64_t(int32_t(int64_t(getReg(hart, rs1)) >> (rawimm & 31u))));
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
				virtWrite<uint8_t>(hart, addr, getReg(hart, rs2));
				break;
			case 1u: // sh
				virtWrite<uint16_t>(hart, addr, getReg(hart, rs2));
				break;
			case 2u: // sw
				virtWrite<uint32_t>(hart, addr, getReg(hart, rs2));
				break;
			case 3u: // sd
				virtWrite<uint64_t>(hart, addr, getReg(hart, rs2));
				break;
			default:
				panic("Unknown store");
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
		case 0x73u: // SYSTEM
		{
			uint32_t funct3 = (inst >> 12u) & 0x7u;
			switch(funct3)
			{
			case 1u: // CSRRW
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				unsigned int rs1 = (inst >> 15u) & 31u;
				uint64_t rs1val = getReg(hart, rs1);
				if (rd != 0u) { // No getCSR side effect if rd is zero
					setReg(hart, rd, getCSR(hart, csr));
				}
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
				setCSR(hart, csr, csrval & ~rs1val);
				break;
			}
			case 5u: // CSRRWI
			{
				uint16_t csr = inst >> 20u;
				unsigned int rd = (inst >> 7u) & 31u;
				if (rd != 0u) { // No getCSR side effect if rd is zero
					setReg(hart, rd, getCSR(hart, csr));
				}
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
			panic("Unknown instruction");
		}

		hart->pc += 4;
	}
}
