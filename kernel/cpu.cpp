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

static bool mmu_load(HartState *hart, uint64_t addr, uint8_t bytes, uint64_t *result) {
    auto res = mmu_translate(hart, addr);
    if (!res.pageoff_mask) {
        // TODO: fault
        return false;
    }
    PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
    void* virt_addr = phys_to_virt<void>(phys);

    switch (bytes) {
        case 1: *result = *(uint8_t*)virt_addr; break;
        case 2: *result = *(uint16_t*)virt_addr; break;
        case 4: *result = *(uint32_t*)virt_addr; break;
        case 8: *result = *(uint64_t*)virt_addr; break;
        default: panic("Unsupported load size");
    }
    return true;
}

static bool mmu_store(HartState *hart, uint64_t addr, uint8_t bytes, uint64_t value) {
    auto res = mmu_translate(hart, addr);
    if (!res.pageoff_mask) {
        // TODO: fault
        return false;
    }
    PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
    void* virt_addr = phys_to_virt<void>(phys);

    switch (bytes) {
        case 1: *(uint8_t*)virt_addr = value; break;
        case 2: *(uint16_t*)virt_addr = value; break;
        case 4: *(uint32_t*)virt_addr = value; break;
        case 8: *(uint64_t*)virt_addr = value; break;
        default: panic("Unsupported store size");
    }
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
	switch (csr) {
	default:
		panic("Unknown CSR 0x%03x", csr);
	}

	return 0;
}

static void setCSR(HartState *hart, uint16_t csr, uint64_t value)
{
	switch (csr) {
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
		panic("Unknown CSR 0x%03x", csr);
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
			const uint16_t inst = inst16;
			printf("Instruction: %04x\n", inst);

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
				uint16_t imm40 = (inst >> 2) & 0x1F,
						 imm5  = (inst >> 12) & 1;

				int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, uint64_t(imm));
			} else if ((inst & 0xEF83) == 0x6101) { // c.addi16sp
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
			} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'01) { // c.lui
				uint16_t imm1612 = (inst >> 2) & 0x1F,
						 imm17   = (inst >> 12) & 1;

				int32_t imm = int32_t(((imm17 << 17) | (imm1612 << 12)) << 14) >> 14;
				uint32_t rd = (inst >> 7) & 0x1F;
				setReg(hart, rd, uint64_t(imm));
			} else if ((inst & 0xF003) == 0x8002) { // c.mv
				uint32_t rd = (inst >> 7) & 0x1F;
				uint32_t rs2 = (inst >> 2) & 0x1F;
				setReg(hart, rd, getReg(hart, rs2));
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
		printf("Instruction: %04x\n", inst);

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
		case 0x1Bu: // integer immediate (64-bit)
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int64_t imm = int32_t(inst) >> 20u;
			uint64_t rawimm = inst >> 20u;

			switch (funct3) {
				case 0x0u: // addiw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1) + imm)));
					break;
				case 0x1u: // slliw
					setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1) << (rawimm & 31u))));
					break;
				case 0x5u: // srliw/sraiw
					if ((rawimm >> 5u) == 0) // srliw
						setReg(hart, rd, int64_t(int32_t(uint32_t(getReg(hart, rs1)) >> (rawimm & 31u))));
					else if ((rawimm >> 5u) == 0x20) // sraiw
						setReg(hart, rd, int64_t(int32_t(getReg(hart, rs1)) >> (rawimm & 31u)));
					else
						panic("Shift not supported");
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
		case 0x37u: // lui
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;
			setReg(hart, rd, int64_t(int32_t(inst & 0xFFFFF000u)));
			break;
		}
		case 0x33u: // integer register-register
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t funct7 = (inst >> 25u) & 127u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;

			uint64_t rs1val = getReg(hart, rs1);
			uint64_t rs2val = getReg(hart, rs2);

			if (funct7 == 1) { // M extension
				panic("M extension not implemented");
			}

			switch (funct3) {
				case 0x0u: // add/sub
					if (funct7 == 0)
						setReg(hart, rd, rs1val + rs2val);
					else if (funct7 == 0x20)
						setReg(hart, rd, rs1val - rs2val);
					else
						panic("Unknown add/sub funct7");
					break;
				case 0x1u: // sll
					setReg(hart, rd, rs1val << (rs2val & 63u));
					break;
				case 0x2u: // slt
					setReg(hart, rd, (int64_t(rs1val) < int64_t(rs2val)) ? 1u : 0u);
					break;
				case 0x3u: // sltu
					setReg(hart, rd, (rs1val < rs2val) ? 1u : 0u);
					break;
				case 0x4u: // xor
					setReg(hart, rd, rs1val ^ rs2val);
					break;
				case 0x5u: // srl/sra
					if (funct7 == 0)
						setReg(hart, rd, rs1val >> (rs2val & 63u));
					else if (funct7 == 0x20)
						setReg(hart, rd, int64_t(rs1val) >> (rs2val & 63u));
					else
						panic("Unknown srl/sra funct7");
					break;
				case 0x6u: // or
					setReg(hart, rd, rs1val | rs2val);
					break;
				case 0x7u: // and
					setReg(hart, rd, rs1val & rs2val);
					break;
			}
			break;
		}
		case 0x3Bu:
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t funct7 = (inst >> 25u) & 127u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;

			uint64_t rs1val = getReg(hart, rs1);
			uint64_t rs2val = getReg(hart, rs2);

			if (funct7 == 1) { // M extension
				panic("M extension not implemented");
			}

			switch (funct3) {
				case 0x0u: // addw/subw
					if (funct7 == 0)
						setReg(hart, rd, int64_t(int32_t(rs1val + rs2val)));
					else if (funct7 == 0x20)
						setReg(hart, rd, int64_t(int32_t(rs1val - rs2val)));
					else
						panic("Unknown addw/subw funct7");
					break;
				case 0x1u: // sllw
					setReg(hart, rd, int64_t(int32_t(rs1val << (rs2val & 31u))));
					break;
				case 0x5u: // srlw/sraw
					if (funct7 == 0)
						setReg(hart, rd, int64_t(int32_t(uint32_t(rs1val) >> (rs2val & 31u))));
					else if (funct7 == 0x20)
						setReg(hart, rd, int64_t(int32_t(rs1val) >> (rs2val & 31u)));
					else
						panic("Unknown srlw/sraw funct7");
					break;
				default:
					panic("Unsupported instruction");
			}
			break;
		}
		case 0x6fu: // jal
		{
			uint32_t rd = (inst >> 7u) & 0x1Fu;

			uint32_t imm20   = (inst >> 31u) & 1u;
			uint32_t imm10_1 = (inst >> 21u) & 0x3FFu;
			uint32_t imm11   = (inst >> 20u) & 1u;
			uint32_t imm19_12 = (inst >> 12u) & 0xFFu;

			uint32_t imm = (imm20 << 20u) | (imm19_12 << 12u) | (imm11 << 11u) | (imm10_1 << 1u);
			int32_t offset = int32_t(imm << 11u) >> 11u;

			setReg(hart, rd, hart->pc + 4);
			hart->pc += offset;
			continue;
		}
		case 0x67u: // jalr
		{
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int64_t imm = int32_t(inst) >> 20u;

			uint64_t target = (getReg(hart, rs1) + imm) & ~1ULL;
			setReg(hart, rd, hart->pc + 4);
			hart->pc = target;
			continue;
		}
		case 0x63u: // branch
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;
			uint64_t rs1val = getReg(hart, rs1);
			uint64_t rs2val = getReg(hart, rs2);

			uint32_t imm12 = (inst >> 31u) & 1u;
			uint32_t imm10_5 = (inst >> 25u) & 0x3Fu;
			uint32_t imm4_1 = (inst >> 8u) & 0xFu;
			uint32_t imm11 = (inst >> 7u) & 1u;

			uint32_t imm = (imm12 << 12u) | (imm11 << 11u) | (imm10_5 << 5u) | (imm4_1 << 1u);
			int32_t offset = int32_t(imm << 19u) >> 19u;

			bool taken = false;
			switch (funct3) {
				case 0x0u: // beq
					if (rs1val == rs2val)
						taken = true;
					break;
				case 0x1u: // bne
					if (rs1val != rs2val)
						taken = true;
					break;
				case 0x4u: // blt
					if (int64_t(rs1val) < int64_t(rs2val))
						taken = true;
					break;
				case 0x5u: // bge
					if (int64_t(rs1val) >= int64_t(rs2val))
						taken = true;
					break;
				case 0x6u: // bltu
					if (rs1val < rs2val)
						taken = true;
					break;
				case 0x7u: // bgeu
					if (rs1val >= rs2val)
						taken = true;
					break;
				default:
					panic("Unknown branch instruction");
			}

			if (taken) {
				hart->pc += offset;
				continue;
			}
			break;
		}
		case 0x03u: // load
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rd = (inst >> 7u) & 31u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			int64_t imm = int32_t(inst) >> 20u;

			uint64_t addr = getReg(hart, rs1) + imm;
			uint64_t value;
			bool success = false;

			switch (funct3) {
				case 0x0u: // lb
					success = mmu_load(hart, addr, 1, &value);
					value = int64_t(int8_t(value));
					break;
				case 0x1u: // lh
					success = mmu_load(hart, addr, 2, &value);
					value = int64_t(int16_t(value));
					break;
				case 0x2u: // lw
					success = mmu_load(hart, addr, 4, &value);
					value = int64_t(int32_t(value));
					break;
				case 0x3u: // ld
					success = mmu_load(hart, addr, 8, &value);
					break;
				case 0x4u: // lbu
					success = mmu_load(hart, addr, 1, &value);
					break;
				case 0x5u: // lhu
					success = mmu_load(hart, addr, 2, &value);
					break;
				case 0x6u: // lwu
					success = mmu_load(hart, addr, 4, &value);
					break;
				default:
					panic("Unknown load instruction");
			}
			if (!success) {
				panic("Load fault");
			}
			setReg(hart, rd, value);
			break;
		}
		case 0x23u: // store
		{
			uint32_t funct3 = (inst >> 12u) & 7u;
			uint32_t rs1 = (inst >> 15u) & 31u;
			uint32_t rs2 = (inst >> 20u) & 31u;

			uint32_t imm11_5 = (inst >> 25u) & 0x7Fu;
			uint32_t imm4_0 = (inst >> 7u) & 0x1Fu;
			int32_t imm = int32_t((imm11_5 << 5u) | imm4_0) << 20u >> 20u;

			uint64_t addr = getReg(hart, rs1) + imm;
			uint64_t value = getReg(hart, rs2);
			bool success = false;

			switch (funct3) {
				case 0x0u: // sb
					success = mmu_store(hart, addr, 1, value);
					break;
				case 0x1u: // sh
					success = mmu_store(hart, addr, 2, value);
					break;
				case 0x2u: // sw
					success = mmu_store(hart, addr, 4, value);
					break;
				case 0x3u: // sd
					success = mmu_store(hart, addr, 8, value);
					break;
				default:
					panic("Unknown store instruction");
			}

			if (!success) {
				panic("Store fault");
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
