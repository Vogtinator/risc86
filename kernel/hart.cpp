#include <math.h>
#include <stdint.h>
#include <stdatomic.h> // stdatomic.h needs stdint.h included before
#include <stdio.h>

#include "hpet.h"
#include "loaderapi.h"
#include "percpu.h"
#include "rvmmu.h"
#include "sbi.h"
#include "utils.h"
#include "x86interrupts.h"

void Hart::handleInterrupt(uint64_t cause, uint64_t stval)
{
	this->scause = cause;
	this->stval = stval;
	this->sepc = this->pc;

	uint64_t sstatus = this->sstatus;

	// Set sstatus.spie to sstatus.sie
	if ((sstatus & SSTATUS_SIE) != 0u)
		sstatus |= SSTATUS_SPIE;
	else
		sstatus &= ~SSTATUS_SPIE;

	// Clear mstatus.mie
	sstatus &= ~SSTATUS_SIE;

	// Set sstatus.spp to mode == MODE_USER
	if (this->mode == Hart::MODE_USER)
		sstatus &= ~SSTATUS_SPP;
	else
		sstatus |= SSTATUS_SPP;

	this->sstatus = sstatus;
	this->pc = this->stvec;
	if (this->mode != Hart::MODE_SUPERVISOR)
		getPerCPU()->x86mmu.switchPrivileges(X86MMU::Priv::Supervisor);

	this->mode = Hart::MODE_SUPERVISOR;
	this->lr_sc_pending = false;
}

void Hart::handleSRET()
{
	uint64_t sstatus = this->sstatus;

	// Set sstatus.sie to sstatus.spie
	if (sstatus & SSTATUS_SPIE)
		sstatus |= SSTATUS_SIE;
	else
		sstatus &= ~SSTATUS_SIE;

	// Go into sstatus.spp mode
	auto oldMode = this->mode;
	this->mode = (sstatus & SSTATUS_SPP) ? Hart::MODE_SUPERVISOR : Hart::MODE_USER;
	if (this->mode != oldMode)
		getPerCPU()->x86mmu.switchPrivileges(this->mode == Hart::MODE_SUPERVISOR
		                                     ? X86MMU::Priv::Supervisor : X86MMU::Priv::User);

	// Set sstatus.spp to U
	sstatus &= ~SSTATUS_SPP;

	this->sstatus = sstatus;
	this->pc = this->sepc;
	this->lr_sc_pending = false;
}

void Hart::handlePendingInterrupts()
{
	// IMSIC external interrupts
	int extInt = 0;
	// Find lowest pending external interrupt
	for (unsigned int i = 0; i < sizeof(this->eip_64) / sizeof(this->eip_64[0]); ++i) {
		auto iepend = this->eip_64[i] & this->eie_64[i];
		if (iepend) {
			extInt = i * 64 + __builtin_ctzg(iepend);
			break;
		}
	}

	// Check if the ext IRQ is lower than eithreshold
	if (extInt > 0 && (this->eithreshold == 0 || uint64_t(extInt) < this->eithreshold))
		this->stopei = (extInt << 16) | extInt;
	else
		this->stopei = 0;

	if (this->stopei && this->eidelivery == 1)
		this->sip |= SIP_SEIP;
	else
		this->sip &= ~SIP_SEIP;

	// Hart interrupt handling
	uint64_t ipend = this->sip & this->sie;

	if (!ipend) {
		this->stopi = 0;
		return;
	}

	auto hartInterrupt = __builtin_ctzg(ipend);
	this->stopi = (hartInterrupt << 16) | 1;
	if((this->sstatus & SSTATUS_SIE) || this->mode == Hart::MODE_USER)
		handleInterrupt(Hart::SCAUSE_INTERRUPT_BASE + hartInterrupt, 0);
}

template <typename T> __attribute__((warn_unused_result))
bool Hart::virtWritePtr(uint64_t addr, T **ptr)
{
	if (addr & (sizeof(T) - 1)) {
		if (!this->satp) {
			// The kernel has unaligned relocs and writes to them
			// directly with misaligned writes. Faulting here
			// results in an endless loop.
		} else {
			printf("Unaligned write\n");
			handleInterrupt(Hart::SCAUSE_STORE_MISALIGN, addr);
			return false;
		}
	}

	auto res = mmu_translate(this, addr, AccessType::Write);
	if (!res.pageoff_mask) {
		handleInterrupt(Hart::SCAUSE_STORE_PAGE_FAULT, addr);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*ptr = phys_to_virt<T>(phys);
	return true;
}

#if MMU_EMULATION
// Perform an instruction fetch of 16 bits at the given addr.
// Returns false on fault.
__attribute__((warn_unused_result))
bool Hart::fetchInstruction(uint16_t *inst, uint64_t addr)
{
	if (addr & (sizeof(uint16_t) - 1))
		panic("Unaligned instruction fetch");

	// Hacky iTLB: Remember the last translated page
	if (itlb.last_satp == this->satp && itlb.last_virt == (addr & ~0xFFFul)) {
		*inst = *phys_to_virt<uint16_t>(itlb.last_phys + (addr & 0xFFFul));
		return true;
	}

	auto res = mmu_translate(this, addr, AccessType::Exec);
	if (!res.pageoff_mask) {
		handleInterrupt(Hart::SCAUSE_INSTR_PAGE_FAULT, addr);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	itlb.last_virt = addr & ~0xFFFul;
	itlb.last_phys = phys & ~0xFFFul;
	itlb.last_satp = this->satp;
	*inst = *phys_to_virt<uint16_t>(phys);
	return true;
}

// TODO: Use native mov with trap here
template <typename T> __attribute__((warn_unused_result))
bool Hart::virtRead(uint64_t addr, T *value)
{
	if (addr & (sizeof(T) - 1)) {
		printf("Unaligned read\n");
		handleInterrupt(Hart::SCAUSE_LOAD_MISALIGN, addr);
		return false;
	}

	auto res = mmu_translate(this, addr, AccessType::Read);
	if (!res.pageoff_mask) {
		handleInterrupt(Hart::SCAUSE_LOAD_PAGE_FAULT, addr);
		return false;
	}

	PhysAddr phys = res.phys_page_addr + (addr & res.pageoff_mask);
	*value = *phys_to_virt<T>(phys);
	return true;
}

template <typename T> __attribute__((warn_unused_result))
bool Hart::virtWrite(uint64_t addr, T value)
{
	T *ptr;
	if (!virtWritePtr(addr, &ptr))
		return false;

	*ptr = value;
	return true;
}
#else
// Perform an instruction fetch of 16 bits at the given addr.
// Returns false on fault.
__attribute__((warn_unused_result))
bool Hart::fetchInstruction(uint16_t *inst, uint64_t addr)
{
	if (addr & (sizeof(uint16_t) - 1))
		panic("Unaligned instruction fetch");

	uint16_t res = 0;
	bool fault;
	asm volatile("clc\n" // Clear carry flag
	             "movw (%[addr]), %[res]\n" // Perform move
	             // Carry flag is used as output directly (=@ccc)
	             // "setc %[fault]" // Carry flag set by fault handler? -> Fault
	             : [res] "=a" (res), [fault] "=@ccc" (fault)
	             : [addr] "d" (addr) : "flags");

	if (fault) {
		handleInterrupt(Hart::SCAUSE_INSTR_PAGE_FAULT, addr);
		return false;
	}

	*inst = res;
	return true;
}

template <typename T> __attribute__((warn_unused_result))
bool Hart::virtRead(uint64_t addr, T *valuePtr)
{
	T value;
	bool fault;
	asm volatile("clc\n" // Clear carry flag
	             "mov (%[addr]), %[value]\n" // Perform move
	             // Carry flag is used as output directly (=@ccc)
	             // "setc %[fault]" // Carry flag set by fault handler? -> Fault
	             : [value] "=a" (value), [fault] "=@ccc" (fault)
	             : [addr] "d" (addr) : "flags");

	if (fault) {
		// The page fault handler sets stval for us.
		// In the case of unaligned access, this is not necessarily == addr!
		handleInterrupt(Hart::SCAUSE_LOAD_PAGE_FAULT, this->stval);
		return false;
	}

	*valuePtr = value;
	return true;
}

template <typename T> __attribute__((warn_unused_result))
bool Hart::virtWrite(uint64_t addr, T value)
{
	bool fault;
	asm volatile("clc\n" // Clear carry flag
	             "mov %[value], (%[addr])\n" // Perform move
	             // Carry flag is used as output directly (=@ccc)
	             // "setc %[fault]" // Carry flag set by fault handler? -> Fault
	             : [fault] "=@ccc" (fault)
	             : [value] "a" (value), [addr] "d" (addr) : "flags", "memory");

	if (fault) {
		// The page fault handler sets stval for us.
		// In the case of unaligned access, this is not necessarily == addr!
		handleInterrupt(Hart::SCAUSE_STORE_PAGE_FAULT, this->stval);
		return false;
	}

	return true;
}
#endif

// Dump Hart state using printf
void Hart::dump()
{
	printf("PC: %016lx (vmlinux: %p, offset: %p)\n",
	       this->pc,
	       (void*)(this->pc - kernel_params.kernel_phys + 0xffffffff80000000),
	       (void*)(this->pc - kernel_params.kernel_phys)
	       );

	// Print general regs
	for (int i = 0; i < 32;)
	{
		printf("R%02d: %016lx ", i, this->regs[i]);
		i++;
		printf("R%02d: %016lx\n", i, this->regs[i]);
		i++;
	}

	// Print float/double regs
	// TODO: Does this work?
	for (int i = 0; i < 32;)
	{
		printf("F%02d: %016lx (%04e) ", i, this->fregs[i].x, (this->fregs[i].w.high == ~0u) ? this->fregs->f : this->fregs->d);
		i++;
		printf("F%02d: %016lx (%04e)\n", i, this->fregs[i].x, (this->fregs[i].w.high == ~0u) ? this->fregs->f : this->fregs->d);
		i++;
	}

	// Print CSRs
	struct { uint64_t *ptr; const char *name; } csrs[] = {
#define REG(x) { &this->x, # x }
		REG(sstatus), REG(stvec), REG(sie), REG(sscratch),
		REG(sepc), REG(scause), REG(satp), REG(stimecmp)
#undef REG
	};

	for (auto csr : csrs)
		printf("%8s: %016lx\n", csr.name, *csr.ptr);

	printf("sip: %08lx\n", this->sip);
	printf("stval: %08lx\n", this->stval);
	printf("scounteren: %08x\n", this->scounteren);
}

bool Hart::faultOnFSOff(uint32_t inst)
{
	if (this->sstatus & SSTATUS_FS_MASK)
		return false; // Turned on

	this->handleInterrupt(Hart::SCAUSE_ILLEGAL_INSTRUCTION, inst);
	return true;
}

uint64_t Hart::getCSR(uint16_t csr)
{
	// TODO: Permission checks
	switch (csr) {
	case 0x001u: // fflags pseudo reg
		return this->fcsr & 0b11111;
	case 0x002u: // frm pseudo reg
		return (this->fcsr >> 5) & 0b111;
	case 0x003u:
		return this->fcsr;
	case 0x100u:
		return this->sstatus;
	case 0x104u:
		return this->sie;
	case 0x140u:
		return this->sscratch;
	case 0x141u:
		return this->sepc;
	case 0x142u:
		return this->scause;
	case 0x143u:
		return this->stval;
	case 0x144u:
		return this->sip;
	case 0x151u: { // sireg -> indirect CSRs
		auto numEIRegs = sizeof(this->eie_64) / sizeof(this->eie_64[0]) * 2;
		auto numEIReg = this->siselect & 63;
		if (this->siselect >= 0x80 && this->siselect < 0xC0
		    && numEIReg < numEIRegs && !(numEIReg & 1)) // eip0-eip63
			return this->eip_64[numEIReg / 2];
		else if (this->siselect >= 0xC0 && this->siselect < 0x100
		         && numEIReg < numEIRegs && !(numEIReg & 1)) // eie0-eie63
			return this->eie_64[numEIReg / 2];

		panic("Unknown indirect CSR read 0x%lx", this->siselect);
		return 0;
	}
	case 0x15c:
		return this->stopei;
	case 0x180u:
		return this->satp;
	case 0xc01u:
		return hpetCurrentTime();
	case 0xdb0u:
		return this->stopi;
	default:
		panic("Unknown CSR read 0x%03x", csr);
	}

	return 0;
}

void Hart::setCSR(uint16_t csr, uint64_t value)
{
	// TODO: Permission checks
	switch (csr) {
	case 0x001u: // fflags pseudo reg
		this->fcsr &= ~0b11111ul;
		this->fcsr |= value & 0b11111;
		return;
	case 0x002u: // frm pseudo reg
		this->fcsr &= ~(0b111ul << 5);
		this->fcsr |= (value & 0b111) << 5;
		return;
	case 0x003u:
		this->fcsr = value;
		return;
	case 0x100u:
		this->sstatus = value;
		return;
	case 0x104u:
		this->sie = value;
		return;
	case 0x105u:
		this->stvec = value;
		return;
	case 0x106u:
		this->scounteren = value & 3;
		return;
	case 0x140u:
		this->sscratch = value;
		return;
	case 0x141u:
		this->sepc = value;
		return;
	case 0x144u:
		// Writing a zero to SSIP acks the IPI, other values kept
		if ((this->sip & SIP_SSIP) && !(value & SIP_SSIP)) {
			this->sip &= ~SIP_SSIP;
			markRVIPIHandled();
		}
		return;
	case 0x14du:
		this->stimecmp = value;
		this->sip &= ~SIP_STIP;
		if (!lapicSetTimeout(this->stimecmp))
			this->sip |= SIP_STIP;

		return;
	case 0x150u:
		this->siselect = value;
		return;
	case 0x151u: { // sireg -> indirect CSRs
		auto numEIRegs = sizeof(this->eie_64) / sizeof(this->eie_64[0]) * 2;
		auto numEIReg = this->siselect & 63;
		if (this->siselect == 0x70) {
			if (value == 0 || value == 1)
				this->eidelivery = value;
		} else if (this->siselect == 0x72) {
			this->eithreshold = value;
		} else if (this->siselect >= 0x80 && this->siselect < 0xC0
		           && numEIReg < numEIRegs && !(numEIReg & 1)) { // eip0-eip63
			this->eip_64[numEIReg / 2] = value;
		} else if (this->siselect >= 0xC0 && this->siselect < 0x100
		           && numEIReg < numEIRegs && !(numEIReg & 1)) { // eie0-eie63
			this->eie_64[numEIReg / 2] = value;
		} else
			panic("Unknown indirect CSR write 0x%lx", this->siselect);

		break;
	}
	case 0x15c: {
		if (this->stopei)
			markRVExtInterruptHandled(this->stopei >> 16);

		this->stopei = 0;
		break;
	} case 0x180u:
		if ((value >> 60) == 0 || (value >> 60) == 8) { // Only bare or Sv39
			this->satp = value;
			getPerCPU()->x86mmu.resetContext();
		} else
			panic("Unsupported SATP value %lx", value);
		return;
	default:
		panic("Unknown CSR write 0x%03x", csr);
	}
}

void Hart::runRVCInstruction(uint16_t inst)
{
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

		this->pc += offs;
		return;
	} else if ((inst & 0xE003) == 0x4001) { // c.li
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >> 2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = (inst >> 7) & 0x1F;
		setReg(rd, uint64_t(imm));
	} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'10) { // c.slli
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >> 2) & 0x1F;

		uint16_t imm = (imm5 << 5) | imm40;
		uint32_t rd = (inst >> 7) & 0x1F;
		setReg(rd, getReg(rd) << imm);
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
		setReg(rd, getReg(2) + imm);
	} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'01) { // c.lui (after c.addi16sp)
		uint16_t imm17   = (inst >> 12) & 1,
		        imm1612 = (inst >>  2) & 0x1F;

		int32_t imm = int32_t(((imm17 << 17) | (imm1612 << 12)) << 14) >> 14;
		uint32_t rd = (inst >> 7) & 0x1F;
		setReg(rd, int64_t(imm));
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
		setReg(rd, getReg(2) + uimm);
	} else if ((inst & 0b111'0'11111'00000'11) == 0b000'0'00000'00000'01) { // c.nop (before c.addi)
		// nop
	} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'01) { // c.addi (after c.nop)
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = (inst >> 7) & 0x1F;
		setReg(rd, getReg(rd) + imm);
	} else if ((inst & 0b111'0'00000'00000'11) == 0b001'0'00000'00000'01) { // c.addiw
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = (inst >> 7) & 0x1F;
		setReg(rd, int64_t(int32_t(getReg(rd) + imm)));
	} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'00'000'00000'01) { // c.srli
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		uint16_t imm = (imm5 << 5) | imm40;
		uint32_t rd = ((inst >> 7) & 7) + 8;
		setReg(rd, getReg(rd) >> imm);
	} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'01'000'00000'01) { // c.srai
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		uint16_t imm = (imm5 << 5) | imm40;
		uint32_t rd = ((inst >> 7) & 7) + 8;
		setReg(rd, int64_t(getReg(rd)) >> imm);
	} else if ((inst & 0b111'0'11'000'00000'11) == 0b100'0'10'000'00000'01) { // c.andi
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = ((inst >> 7) & 7) + 8;
		setReg(rd, getReg(rd) & uint64_t(imm));
	} else if ((inst & 0b111'000'000'00'000'11) == 0b011'000'000'00'000'00) { // c.ld
		uint16_t imm53 = (inst >> 10) & 7,
		        imm76 = (inst >>  5) & 3;

		uint16_t off = (imm76 << 6) | (imm53 << 3);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rd  = ((inst >> 2) & 7) + 8;

		uint64_t value;
		if (!virtRead<uint64_t>(getReg(rs1) + off, &value))
			return;

		setReg(rd, value);
	} else if ((inst & 0b111'000'000'00'000'11) == 0b001'000'000'00'000'00) { // c.fld
		if (faultOnFSOff(inst))
			return;

		uint16_t imm53 = (inst >> 10) & 7,
		        imm76 = (inst >>  5) & 3;

		uint16_t off = (imm76 << 6) | (imm53 << 3);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rd  = ((inst >> 2) & 7) + 8;

		double value;
		if (!virtRead(getReg(rs1) + off, &value))
			return;

		setFReg<double>(rd, value);
	} else if ((inst & 0b111'000'000'00'000'11) == 0b010'000'000'00'000'00) { // c.lw
		uint16_t imm53 = (inst >> 10) & 7,
		        imm2  = (inst >>  6) & 1,
		        imm6  = (inst >>  5) & 1;

		uint16_t off = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rd  = ((inst >> 2) & 7) + 8;

		int32_t value;
		if (!virtRead<int32_t>(getReg(rs1) + off, &value))
			return;

		setReg(rd, value);
	} else if ((inst & 0b111'000'000'00'000'11) == 0b110'000'000'00'000'00) { // c.sw
		uint16_t imm53 = (inst >> 10) & 7,
		        imm2  = (inst >>  6) & 1,
		        imm6  = (inst >>  5) & 1;

		uint16_t off = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rs2 = ((inst >> 2) & 7) + 8;

		if (!virtWrite<uint32_t>(getReg(rs1) + off, getReg(rs2)))
			return;
	} else if ((inst & 0b111'000'000'00'000'11) == 0b111'000'000'00'000'00) { // c.sd
		uint16_t imm53 = (inst >> 10) & 7,
		        imm76 = (inst >>  5) & 3;

		uint16_t off = (imm76 << 6) | (imm53 << 3);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rs2 = ((inst >> 2) & 7) + 8;

		if (!virtWrite<uint64_t>(getReg(rs1) + off, getReg(rs2)))
			return;
	} else if ((inst & 0b111'000'000'00'000'11) == 0b101'000'000'00'000'00) { // c.fsd
		if (faultOnFSOff(inst))
			return;

		uint16_t imm53 = (inst >> 10) & 7,
		        imm76 = (inst >>  5) & 3;

		uint16_t off = (imm76 << 6) | (imm53 << 3);

		uint32_t rs1 = ((inst >> 7) & 7) + 8,
		        rs2 = ((inst >> 2) & 7) + 8;

		if (!virtWrite<double>(getReg(rs1) + off, getFReg<double>(rs2)))
			return;
	} else if ((inst & 0b111'1'00000'11111'11) == 0b100'0'00000'00000'10) { // c.jr (before c.mv)
		uint32_t rs1 = (inst >> 7) & 0x1F;

		if (rs1 == 0)
			panic("Reserved c.???");

		this->pc = getReg(rs1);
		return;
	} else if ((inst & 0b111'1'00000'00000'11) == 0b100'0'00000'00000'10) { // c.mv (after c.jr)
		uint32_t rs2 = (inst >> 2) & 0x1F,
		        rd  = (inst >> 7) & 0x1F;

		setReg(rd, getReg(rs2));
	} else if ((inst & 0b111'1'11111'11111'11) == 0b100'1'00000'00000'10) { // c.ebreak
		this->handleInterrupt(Hart::SCAUSE_EBREAK, 0);
		return;
	} else if ((inst & 0b111'1'00000'11111'11) == 0b100'1'00000'00000'10) { // c.jalr (after c.ebreak)
		uint32_t rs1 = (inst >> 7u) & 0x1Fu;
		uint64_t retaddr = this->pc + 2u;
		this->pc = getReg(rs1);
		setReg(1, retaddr);
		return;
	} else if ((inst & 0b111'1'00000'00000'11) == 0b100'1'00000'00000'10) { // c.add (after c.jalr)
		uint32_t rs2 = (inst >> 2) & 0x1F,
		        rd  = (inst >> 7) & 0x1F;

		setReg(rd, getReg(rd) + getReg(rs2));
	} else if ((inst & 0b111'000000'00000'11) == 0b111'000000'00000'10) { // c.sdsp
		uint16_t imm53 = (inst >> 10) & 7,
		        imm86 = (inst >>  7) & 7;

		uint16_t off = (imm86 << 6) | (imm53 << 3);

		uint32_t rs2 = (inst >> 2) & 31;

		if (!virtWrite(getReg(2) + off, getReg(rs2)))
			return;
	} else if ((inst & 0b111'000000'00000'11) == 0b101'000000'00000'10) { // c.fsdsp
		if (faultOnFSOff(inst))
			return;

		uint16_t imm53 = (inst >> 10) & 7,
		        imm86 = (inst >>  7) & 7;

		uint16_t off = (imm86 << 6) | (imm53 << 3);

		uint32_t rs2 = (inst >> 2) & 31;

		if (!virtWrite(getReg(2) + off, getFReg<double>(rs2)))
			return;
	} else if ((inst & 0b111'000000'00000'11) == 0b110'000000'00000'10) { // c.swsp
		uint16_t imm52 = (inst >>  9) & 0xF,
		        imm76 = (inst >>  7) & 3;

		uint16_t off = (imm76 << 6) | (imm52 << 2);

		uint32_t rs2 = (inst >> 2) & 31;

		if (!virtWrite<uint32_t>(getReg(2) + off, uint32_t(getReg(rs2))))
			return;
	} else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'10) { // c.ldsp
		uint16_t imm5  = (inst >> 12) & 1,
		        imm43 = (inst >>  5) & 3,
		        imm86 = (inst >>  2) & 7;

		uint16_t off = (imm86 << 6) | (imm5 << 5) | (imm43 << 3);

		uint32_t rd = (inst >> 7) & 31;

		if (rd == 0)
			panic("Reserved instruction %x", inst);

		uint64_t value;
		if (!virtRead(getReg(2) + off, &value))
			return;

		setReg(rd, value);
	} else if ((inst & 0b111'0'00000'00000'11) == 0b001'0'00000'00000'10) { // c.fldsp
		if (faultOnFSOff(inst))
			return;

		uint16_t imm5  = (inst >> 12) & 1,
		        imm43 = (inst >>  5) & 3,
		        imm86 = (inst >>  2) & 7;

		uint16_t off = (imm86 << 6) | (imm5 << 5) | (imm43 << 3);

		uint32_t rd = (inst >> 7) & 31;

		double value;
		if (!virtRead(getReg(2) + off, &value))
			return;

		setFReg(rd, value);
	} else if ((inst & 0b111'0'00000'00000'11) == 0b010'0'00000'00000'10) { // c.lwsp
		uint16_t imm5  = (inst >> 12) & 1,
		        imm42 = (inst >>  4) & 7,
		        imm76 = (inst >>  2) & 3;

		uint16_t off = (imm76 << 6) | (imm5 << 5) | (imm42 << 2);

		uint32_t rd = (inst >> 7) & 31;

		if (rd == 0)
			panic("Reserved instruction %x", inst);

		int32_t value;
		if (!virtRead(getReg(2) + off, &value))
			return;

		setReg(rd, value);
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'00'000'01) { // c.sub
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, getReg(rd) - getReg(rs2));
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'01'000'01) { // c.xor
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, getReg(rd) ^ getReg(rs2));
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'10'000'01) { // c.or
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, getReg(rd) | getReg(rs2));
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'0'11'000'11'000'01) { // c.and
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, getReg(rd) & getReg(rs2));
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'1'11'000'00'000'01) { // c.subw
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, int64_t(int32_t(getReg(rd)) - int32_t(getReg(rs2))));
	} else if ((inst & 0b111'1'11'000'11'000'11) == 0b100'1'11'000'01'000'01) { // c.addw
		uint32_t rs2 = ((inst >> 2) & 7) + 8,
		        rd  = ((inst >> 7) & 7) + 8;

		setReg(rd, int64_t(int32_t(getReg(rd)) + int32_t(getReg(rs2))));
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

		if (getReg(rs1) == 0) {
			this->pc += imm;
			return;
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

		if (getReg(rs1) != 0) {
			this->pc += imm;
			return;
		}
	} else {
		panic("Unknown instruction %04x", inst);
	}

	this->pc += 2;
}

void Hart::runInstruction(uint32_t inst)
{
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

		uint64_t addr = getReg(rs1) + imm;
		switch (funct3)
		{
		case 0u: { // lb
			int8_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, int64_t(val));
			break;
		}
		case 1u: { // lh
			int16_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, int64_t(val));
			break;
		}
		case 2u: { // lw
			int32_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, int64_t(val));
			break;
		}
		case 3u: { // ld
			int64_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, val);
			break;
		}
		case 4u: { // lbu
			uint8_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, uint64_t(val));
			break;
		}
		case 5u: { // lhu
			uint16_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, uint64_t(val));
			break;
		}
		case 6u: { // lwu
			uint32_t val;
			if (!virtRead(addr, &val))
				return;

			setReg(rd, uint64_t(val));
			break;
		}
		default:
			panic("Unknown load instruction");
		}
		break;
	}
	case 0x07u: // FP load
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rd = (inst >> 7u) & 31u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		int32_t imm = int32_t(inst) >> 20u;

		uint64_t addr = getReg(rs1) + imm;
		switch (funct3)
		{
		case 0b010: { // flw
			if (faultOnFSOff(inst))
				return;

			float val;
			if (!virtRead(addr, &val))
				return;

			setFReg(rd, val);
			break;
		}
		case 0b011: { // fld
			if (faultOnFSOff(inst))
				return;

			double val;
			if (!virtRead(addr, &val))
				return;

			setFReg(rd, val);
			break;
		}
		default:
			panic("Unknown FP load instruction %x", inst);
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
			setReg(rd, int64_t(getReg(rs1)) + imm);
			break;
		case 0x1u: // slli
			if ((rawimm >> 6u) == 0) // slli
				setReg(rd, getReg(rs1) << (rawimm & 63u));
			else
				panic("Shift not supported");

			break;
		case 0x2u: // slti
			setReg(rd, (int64_t(getReg(rs1)) < imm) ? 1u : 0u);
			break;
		case 0x3u: // sltiu
			setReg(rd, (getReg(rs1) < uint64_t(imm)) ? 1u : 0u);
			break;
		case 0x4u: // xori
			setReg(rd, getReg(rs1) ^ imm);
			break;
		case 0x5u: // sr(l,a)i
			if ((rawimm >> 6u) == 0) // srli
				setReg(rd, getReg(rs1) >> (rawimm & 63u));
			else if ((rawimm >> 6u) == 0x10u) // srai
				setReg(rd, int64_t(getReg(rs1)) >> (rawimm & 63u));
			else
				panic("Shift not supported");

			break;
		case 0x6u: // ori
			setReg(rd, getReg(rs1) | imm);
			break;
		case 0x7u: // andi
			setReg(rd, getReg(rs1) & imm);
			break;
		default:
			panic("Unsupported instruction");
		}
		break;
	}
	case 0x17u: // auipc
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		setReg(rd, this->pc + int32_t(inst & 0xFFFFF000u));
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
			setReg(rd, int64_t(int32_t(int64_t(getReg(rs1)) + imm)));
			break;
		case 0x1u: // slliw
			if ((rawimm >> 5u) == 0) // slliw
				setReg(rd, int64_t(int32_t(getReg(rs1) << (rawimm & 31u))));
			else
				panic("Shift not supported");

			break;
		case 0x5u: // sr(l,a)iw
			if ((rawimm >> 5u) == 0) // srliw
				setReg(rd, int32_t(uint32_t(getReg(rs1)) >> (rawimm & 31u)));
			else if ((rawimm >> 6u) == 0x10u) // sraiw
				setReg(rd, int32_t(uint32_t(getReg(rs1))) >> (rawimm & 31u));
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
		uint64_t addr = getReg(rs1) + imm;
		switch (funct3)
		{
		case 0u: // sb
			if (!virtWrite<uint8_t>(addr, getReg(rs2)))
				return;
			break;
		case 1u: // sh
			if (!virtWrite<uint16_t>(addr, getReg(rs2)))
				return;
			break;
		case 2u: // sw
			if (!virtWrite<uint32_t>(addr, getReg(rs2)))
				return;
			break;
		case 3u: // sd
			if (!virtWrite<uint64_t>(addr, getReg(rs2)))
				return;
			break;
		default:
			panic("Unknown store");
		}
		break;
	}
	case 0x27u: // FP store
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		int32_t imm = ((int32_t(inst) >> 25u) << 5u) | ((inst >> 7u) & 0x1Fu);

		uint64_t addr = getReg(rs1) + imm;

		switch (funct3)
		{
		case 0b010: { // fsw
			if (faultOnFSOff(inst))
				return;

			if (!virtWrite(addr, getFReg<float>(rs2)))
				return;
			break;
		}
		case 0b011: { // fsd
			if (faultOnFSOff(inst))
				return;

			if (!virtWrite(addr, getFReg<double>(rs2)))
				return;
			break;
		}
		default:
			panic("Unknown FP store instruction %x", inst);
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

		switch((funct7 << 4u) | funct3)
		{
		case 0x002u: // amoadd.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t val = atomic_fetch_add(ptr, uint32_t(getReg(rs2)));
			setReg(rd, int64_t(int32_t(val)));
			break;
		}
		case 0x003u: // amoadd.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val = atomic_fetch_add(ptr, getReg(rs2));
			setReg(rd, val);
			break;
		}
		case 0x042u: // amoswap.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t val = atomic_exchange(ptr, uint32_t(getReg(rs2)));
			setReg(rd, int64_t(int32_t(val)));
			break;
		}
		case 0x043u: // amoswap.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val = atomic_exchange(ptr, getReg(rs2));
			setReg(rd, val);
			break;
		}
		/* Emulating LR/SC using weaker compare-exchange is technically wrong
		 * but probably fine in most cases, as most high-level languages
		 * expose only CAS semantics anyway. */
		case 0x082u: // lr.w
		{
			if (rs2 != 0u)
				panic("lr with non-zero");

			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			lr_sc_pending = true;
			lr_sc_address = addr;
			lr_sc_value = int32_t(*ptr);
			setReg(rd, lr_sc_value);
			break;
		}
		case 0x0c2u: // sc.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t expected = lr_sc_value, desired = getReg(rs2);
			if (lr_sc_pending && lr_sc_address == addr
			    && atomic_compare_exchange_strong(ptr, &expected, desired))
				setReg(rd, 0u); // Success
			else
				setReg(rd, 1u); // Fail

			break;
		}
		case 0x083u: // lr.d
		{
			if (rs2 != 0u)
				panic("lr with non-zero");

			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			lr_sc_pending = true;
			lr_sc_address = addr;
			lr_sc_value = *ptr;
			setReg(rd, lr_sc_value);
			break;
		}
		case 0x0c3u: // sc.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t expected = lr_sc_value, desired = getReg(rs2);
			if (lr_sc_pending && lr_sc_address == addr
			    && atomic_compare_exchange_strong(ptr, &expected, desired))
				setReg(rd, 0u); // Success
			else
				setReg(rd, 1u); // Fail

			break;
		}
		case 0x103u: // amoxor.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val = atomic_fetch_xor(ptr, getReg(rs2));
			setReg(rd, val);
			break;
		}
		case 0x202u: // amoor.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t val = atomic_fetch_or(ptr, uint32_t(getReg(rs2)));
			setReg(rd, int64_t(int32_t(val)));
			break;
		}
		case 0x203u: // amoor.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val = atomic_fetch_or(ptr, getReg(rs2));
			setReg(rd, val);
			break;
		}
		case 0x302u: // amoand.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t val = atomic_fetch_and(ptr, uint32_t(getReg(rs2)));
			setReg(rd, int64_t(int32_t(val)));
			break;
		}
		case 0x303u: // amoand.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val = atomic_fetch_and(ptr, getReg(rs2));
			setReg(rd, val);
			break;
		}
		case 0x702u: // amomaxu.w
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint32_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint32_t val;
			for (;;) {
				val = *ptr;
				uint32_t desired = max(val, uint32_t(getReg(rs2)));
				if (atomic_compare_exchange_weak(ptr, &val, desired))
					break;
			}

			setReg(rd, int64_t(int32_t(val)));
			break;
		}
		case 0x703u: // amomaxu.d
		{
			uint64_t addr = getReg(rs1);
			_Atomic(uint64_t) *ptr;
			if (!virtWritePtr(addr, &ptr))
				return;

			uint64_t val;
			for (;;) {
				val = *ptr;
				uint64_t desired = max(val, getReg(rs2));
				if (atomic_compare_exchange_weak(ptr, &val, desired))
					break;
			}

			setReg(rd, val);
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

		switch((funct7 << 4u) | funct3)
		{
		case 0x000u: // add
			setReg(rd, getReg(rs1) + getReg(rs2));
			break;
		case 0x001u: // sll
			setReg(rd, getReg(rs1) << (getReg(rs2) & 63u));
			break;
		case 0x002u: // slt
			setReg(rd, int64_t(getReg(rs1)) < int64_t(getReg(rs2)) ? 1u : 0u);
			break;
		case 0x003u: // sltu
			setReg(rd, (getReg(rs1) < getReg(rs2)) ? 1u : 0u);
			break;
		case 0x004u: // xor
			setReg(rd, getReg(rs1) ^ getReg(rs2));
			break;
		case 0x005u: // srl
			setReg(rd, getReg(rs1) >> (getReg(rs2) & 63u));
			break;
		case 0x006u: // or
			setReg(rd, getReg(rs1) | getReg(rs2));
			break;
		case 0x007u: // and
			setReg(rd, getReg(rs1) & getReg(rs2));
			break;
		case 0x010u: // mul
			setReg(rd, getReg(rs1) * getReg(rs2));
			break;
		case 0x011u: // mulh
			setReg(rd, (__int128_t(int64_t(getReg(rs1))) * __int128_t(int64_t(getReg(rs2)))) >> 64);
			break;
		case 0x012u: // mulhsu
			setReg(rd, (__int128_t(int64_t(getReg(rs1))) * __uint128_t(getReg(rs2))) >> 64);
			break;
		case 0x013u: // mulhu
			setReg(rd, (__uint128_t(getReg(rs1)) * __uint128_t(getReg(rs2))) >> 64);
			break;
		case 0x014u: // div
			if (getReg(rs2) == 0)
				setReg(rd, ~uint64_t(0));
			else if (int64_t(getReg(rs1)) == INT64_MIN && int64_t(getReg(rs2)) == -1)
				setReg(rd, uint64_t(INT64_MIN));
			else
				setReg(rd, int64_t(getReg(rs1)) / int64_t(getReg(rs2)));

			break;
		case 0x015u: // divu
			if (getReg(rs2) == 0)
				setReg(rd, ~uint64_t(0));
			else
				setReg(rd, getReg(rs1) / getReg(rs2));

			break;
		case 0x016u: // rem
			// TODO: Signedness correct?
			if (getReg(rs2) == 0)
				setReg(rd, getReg(rs1));
			else if (int64_t(getReg(rs1)) == INT64_MIN && int64_t(getReg(rs2)) == -1)
				setReg(rd, 0);
			else
				setReg(rd, int64_t(getReg(rs1)) % int64_t(getReg(rs2)));
			break;
		case 0x017u: // remu
			if (getReg(rs2) == 0)
				setReg(rd, getReg(rs1));
			else
				setReg(rd, getReg(rs1) % getReg(rs2));
			break;
		case 0x200u: // sub
			setReg(rd, getReg(rs1) - getReg(rs2));
			break;
		case 0x205u: // sra
			setReg(rd, int64_t(getReg(rs1)) >> (getReg(rs2) & 63u));
			break;
		default:
			panic("Unknown reg-reg instruction");
		}
		break;
	}
	case 0x37u: // lui
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		setReg(rd, int64_t(int32_t(inst & 0xFFFFF000u)));
		break;
	}
	case 0x3Bu: // integer register (RV64)
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rd = (inst >> 7u) & 31u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		uint32_t funct7 = inst >> 25u;

		switch((funct7 << 4u) | funct3)
		{
		case 0x000u: // addw
			setReg(rd, int64_t(int32_t(getReg(rs1)) + int32_t(getReg(rs2))));
			break;
		case 0x001u: // sllw
			setReg(rd, int64_t(int32_t(getReg(rs1)) << (getReg(rs2) & 31u)));
			break;
		case 0x005u: // srlw
			setReg(rd, int64_t(int32_t(uint32_t(getReg(rs1)) >> (getReg(rs2) & 31u))));
			break;
		case 0x010u: // mulw
			setReg(rd, int64_t(int32_t(getReg(rs1)) * int32_t(getReg(rs2))));
			break;
		case 0x014u: // divw
			if (uint32_t(getReg(rs2)) == 0)
				setReg(rd, ~uint64_t(0));
			else if (int32_t(getReg(rs1)) == INT32_MIN && int32_t(getReg(rs2)) == -1)
				setReg(rd, int64_t(INT32_MIN));
			else
				setReg(rd, int64_t(int32_t(getReg(rs1)) / int32_t(getReg(rs2))));
			break;
		case 0x015u: // divuw
			if (uint32_t(getReg(rs2)) == 0)
				setReg(rd, ~uint64_t(0));
			else
				setReg(rd, int64_t(int32_t(uint32_t(getReg(rs1)) / uint32_t(getReg(rs2)))));
			break;
		case 0x016u: // remw
			if (uint32_t(getReg(rs2)) == 0)
				setReg(rd, int64_t(int32_t(getReg(rs1))));
			else if (int32_t(getReg(rs1)) == INT32_MIN && int32_t(getReg(rs2)) == -1)
				setReg(rd, 0);
			else
				setReg(rd, int64_t(int32_t(int32_t(getReg(rs1)) % int32_t(getReg(rs2)))));
			break;
		case 0x017u: // remuw
			if (uint32_t(getReg(rs2)) == 0)
				setReg(rd, int64_t(int32_t(getReg(rs1))));
			else
				setReg(rd, int64_t(int32_t(uint32_t(getReg(rs1)) % uint32_t(getReg(rs2)))));
			break;
		case 0x200u: // subw
			setReg(rd, int64_t(int32_t(getReg(rs1)) - int32_t(getReg(rs2))));
			break;
		case 0x205u: // sraw
			setReg(rd, int64_t(int32_t(uint32_t(getReg(rs1))) >> (getReg(rs2) & 31u)));
			break;
		default:
			panic("Unknown 32-bit reg-reg instruction");
		}
		break;
	}
	case 0x43u: // FMADD
	case 0x47u: // FMSUB
	case 0x4Bu: // FNMSUB
	case 0x4Fu: // FNMADD
	{
		if (faultOnFSOff(inst))
			return;

		uint32_t op = (inst >> 2u) & 3u;
		uint32_t rd = (inst >> 7u) & 31u;
		uint32_t rm = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		uint32_t funct2 = (inst >> 25u) & 0b11;
		uint32_t rs3 = (inst >> 27u) & 31u;

		(void) rm; // Rounding modes not implemented

		// Operations common to doubles and floats are deduplicated using this templated lambda.
		// Return true if instruction executed.
		auto commonStuff = [&] <typename T, bool isDouble = sizeof(T) == sizeof(double)> () {
			uint32_t doublebit = isDouble ? 0b1 : 0b0;
			if (op == 0b00 && funct2 == doublebit) { // FMADD.{S,D}
				setFReg<T>(rd, genericFMA(getFReg<T>(rs1), getFReg<T>(rs2), getFReg<T>(rs3)));
			} else if (op == 0b01 && funct2 == doublebit) { // FMSUB.{S,D}
				setFReg<T>(rd, genericFMA(getFReg<T>(rs1), getFReg<T>(rs2), -getFReg<T>(rs3)));
			} else if (op == 0b10 && funct2 == doublebit) { // FNMSUB.{S,D}
				setFReg<T>(rd, genericFMA(-getFReg<T>(rs1), getFReg<T>(rs2), getFReg<T>(rs3)));
				} else if (op == 0b11 && funct2 == doublebit) { // FNMADD.{S,D}
				setFReg<T>(rd, genericFMA(-getFReg<T>(rs1), getFReg<T>(rs2), -getFReg<T>(rs3)));
				} else
				return false;

			return true;
		};

		if (commonStuff.operator()<float>() || commonStuff.operator()<double>())
			break;
		else
			panic("Unknown FP instruction %x", inst);

		break;
	}
	case 0x53u: // FP
	{
		if (faultOnFSOff(inst))
			return;

		uint32_t rd = (inst >> 7u) & 31u;
		uint32_t rm = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		uint32_t funct7 = inst >> 25u;

		// Operations common to doubles and floats are deduplicated using this templated lambda.
		// Return true if instruction executed.
		auto commonStuff = [&] <typename T, bool isDouble = sizeof(T) == sizeof(double)> () {
			uint32_t doublebit = isDouble ? 0b1 : 0b0;
			if (funct7 == (0b0001000 | doublebit)) { // FMUL.{S,D}
				setFReg<T>(rd, getFReg<T>(rs1) * getFReg<T>(rs2));
			} else if (funct7 == (0b0001100 | doublebit)) { // FDIV.{S,D}
				setFReg<T>(rd, getFReg<T>(rs1) / getFReg<T>(rs2));
			} else if (funct7 == (0b0000000 | doublebit)) { // FADD.{S,D}
				setFReg<T>(rd, getFReg<T>(rs1) + getFReg<T>(rs2));
			} else if (funct7 == (0b0000100 | doublebit)) { // FSUB.{S,D}
				setFReg<T>(rd, getFReg<T>(rs1) - getFReg<T>(rs2));
			} else if (funct7 == (0b0101100 | doublebit) && rs2 == 0b00000) { // FSQRT.{S,D}
				setFReg<T>(rd, genericSqrt(getFReg<T>(rs1)));
			} else if (funct7 == (0b1010000 | doublebit)) {
				if (rm == 0b000) // FLE.{S,D}
					setReg(rd, getFReg<T>(rs1) <= getFReg<T>(rs2));
				else if (rm == 0b001) // FLT.{S,D}
					setReg(rd, getFReg<T>(rs1) < getFReg<T>(rs2));
				else if (rm == 0b010) // FEQ.{S,D}
					setReg(rd, getFReg<T>(rs1) == getFReg<T>(rs2));
				else
					return false;
			} else if (funct7 == (0b0010100 | doublebit) && rm == 0b000) { // FMIN.{S,D}
				setFReg<T>(rd, min(getFReg<T>(rs1), getFReg<T>(rs2)));
			} else if (funct7 == (0b0010100 | doublebit) && rm == 0b001) { // FMAX.{S,D}
				setFReg<T>(rd, max(getFReg<T>(rs1), getFReg<T>(rs2)));
			} else if (funct7 == (0b1100000 | doublebit) && rs2 == 0b00000) { // FCVT.W.{S,D}
				setReg(rd, int32_t(getFReg<T>(rs1)));
			} else if (funct7 == (0b1100000 | doublebit) && rs2 == 0b00001) { // FCVT.WU.{S,D}
				setReg(rd, uint32_t(getFReg<T>(rs1)));
			} else if (funct7 == (0b1100000 | doublebit) && rs2 == 0b00010) { // FCVT.L.{S,D}
				setReg(rd, int64_t(getFReg<T>(rs1)));
			} else if (funct7 == (0b1100000 | doublebit) && rs2 == 0b00011) { // FCVT.LU.{S,D}
				setReg(rd, uint64_t(getFReg<T>(rs1)));
			} else if (funct7 == (0b1101000 | doublebit) && rs2 == 0b00000) { // FCVT.{S,D}.W
				setFReg<T>(rd, int32_t(getReg(rs1)));
			} else if (funct7 == (0b1101000 | doublebit) && rs2 == 0b00001) { // FCVT.{S,D}.WU
				setFReg<T>(rd, uint32_t(getReg(rs1)));
			} else if (funct7 == (0b1101000 | doublebit) && rs2 == 0b00010) { // FCVT.{S,D}.L
				setFReg<T>(rd, int64_t(getReg(rs1)));
			} else if (funct7 == (0b1101000 | doublebit) && rs2 == 0b00011) { // FCVT.{S,D}.LU
				setFReg<T>(rd, uint64_t(getReg(rs1)));
			} else if (funct7 == (0b1110000 | doublebit) && rs2 == 0b00000 && rm == 0b001) { // FCLASS.{S,D}
				uint8_t t;
				auto val = getFReg<T>(rs1);
				switch (fpclassify(val)) {
				case FP_INFINITE:
					t = val < 0 ? 0 : 7;
					break;
				case FP_NORMAL:
					t = val < 0 ? 1 : 6;
					break;
				case FP_SUBNORMAL:
					t = val < 0 ? 2 : 5;
					break;
				case FP_ZERO:
					t = signbit(val) ? 3 : 4;
					break;
				case FP_NAN:
					t = __builtin_issignaling(val) ? 8 : 9;
					break;
				default:
					panic("Unknown FP classification?");
				}
				setReg(rd, 1 << t);
			} else
				return false;

		    return true;
		};

		if (commonStuff.operator()<float>() || commonStuff.operator()<double>()) {
			break;
		} else if (funct7 == 0b0100000 && rs2 == 0b00001) { // FCVT.S.D
			setFReg<float>(rd, getFReg<double>(rs1));
		} else if (funct7 == 0b0100001 && rs2 == 0b00000) { // FCVT.D.S
			setFReg<double>(rd, getFReg<float>(rs1));
		} else if (funct7 == 0b1110000 && rs2 == 0b00000 && rm == 0b000) { // FMV.X.W
			setReg(rd, int32_t(getFRegBitsFloat(rs1)));
		} else if (funct7 == 0b1110001 && rs2 == 0b00000 && rm == 0b000) { // FMV.X.D
			setReg(rd, getFRegBitsDouble(rs1));
		} else if (funct7 == 0b1111000 && rs2 == 0b00000 && rm == 0b000) { // FMV.W.X
			setFRegBitsFloat(rd, getReg(rs1));
		} else if (funct7 == 0b1111001 && rs2 == 0b00000 && rm == 0b000) { // FMV.D.X
			setFRegBitsDouble(rd, getReg(rs1));
		} else if (funct7 == 0b0010000 && rm == 0b000) { // FSGNJ.S
			setFRegBitsFloat(rd, (getFRegBitsFloat(rs2) & (1u << 31)) | (getFRegBitsFloat(rs1) & ~(1u << 31)));
		} else if (funct7 == 0b0010000 && rm == 0b001) { // FSGNJN.S
			setFRegBitsFloat(rd, (~getFRegBitsFloat(rs2) & (1u << 31)) | (getFRegBitsFloat(rs1) & ~(1u << 31)));
		} else if (funct7 == 0b0010000 && rm == 0b010) { // FSGNJX.S
			uint32_t sign = (getFRegBitsFloat(rs2) & (1u << 31)) ^ (getFRegBitsFloat(rs1) & (1u << 31));
			setFRegBitsFloat(rd, sign | (getFRegBitsFloat(rs1) & ~(1u << 31)));
		} else if (funct7 == 0b0010001 && rm == 0b000) { // FSGNJ.D
			setFRegBitsDouble(rd, (getFRegBitsDouble(rs2) & (1ul << 63)) | (getFRegBitsDouble(rs1) & ~(1ul << 63)));
		} else if (funct7 == 0b0010001 && rm == 0b001) { // FSGNJN.D
			setFRegBitsDouble(rd, (~getFRegBitsDouble(rs2) & (1ul << 63)) | (getFRegBitsDouble(rs1) & ~(1ul << 63)));
		} else if (funct7 == 0b0010001 && rm == 0b010) { // FSGNJX.D
			uint64_t sign = (getFRegBitsDouble(rs2) & (1ul << 63)) ^ (getFRegBitsDouble(rs1) & (1ul << 63));
			setFRegBitsDouble(rd, sign | (getFRegBitsDouble(rs1) & ~(1ul << 63)));
		} else
			panic("Unknown FP instruction %x", inst);

		break;
	}
	case 0x63u: // branch
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		int64_t rs1vals = int64_t(getReg(rs1));
		int64_t rs2vals = int64_t(getReg(rs2));
		uint64_t rs1valu = getReg(rs1);
		uint64_t rs2valu = getReg(rs2);
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
			this->pc += imm;
			return;
		}
		break;
	}
	case 0x67u: // jalr
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		uint32_t rs1 = (inst >> 15u) & 31u;
		int32_t imm = int32_t(inst) >> 20u;

		uint64_t retaddr = this->pc + 4u;
		this->pc = getReg(rs1) + imm;
		setReg(rd, retaddr);
		return;
	}
	case 0x6fu: // jal
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		uint32_t imm20 = inst >> 31u;
		uint32_t imm101 = (inst >> 21u) & 0x3FFu;
		uint32_t imm11 = (inst >> 20u) & 1u;
		uint32_t imm1912 = (inst >> 12u) & 0xFFu;
		int32_t imm = int32_t(((imm20 << 20u) | (imm1912 << 12u) | (imm11 << 11u) | (imm101 << 1u)) << 11u) >> 11u;
		setReg(rd, this->pc + 4u);
		this->pc += imm;
		return;
	}
	case 0x73u: // SYSTEM
	{
		uint32_t funct3 = (inst >> 12u) & 0x7u;
		switch(funct3)
		{
		case 0u: // Misc stuff
		{
			if (inst == 0x00000073u) { // ecall
				if (this->mode == Hart::MODE_SUPERVISOR)
					handleSBICall(this);
				else {
					this->handleInterrupt(Hart::SCAUSE_ECALL_UMODE, 0);
					return;
				}
			} else if (inst == 0x00100073u) { // ebreak
				// TODO: Check surrounding instructions to properly separate debug ebreaks
				// from semihosting calls.
				if (this->regs[10] == 3) { // semihosting putc
					uint8_t ch;
					if (virtRead(getReg(11), &ch))
						putchar(ch);
					else
						panic("Fault during semihost putc");
					break;
				}
				this->handleInterrupt(Hart::SCAUSE_EBREAK, 0);
			} else if ((inst & 0b1111111'00000'00000'111'11111'1111111) == 0b0001001'00000'00000'000'00000'1110011) { // sfence.vma
				uint32_t rs1 = (inst >> 15) & 31;
				uint32_t rs2 = (inst >> 20) & 31;

				//printf("Doing some fencing for %lx\n", getReg(rs1));

				(void) rs2; // No ASID support
				if (rs1 == 0)
					getPerCPU()->x86mmu.resetContext();
				else
					getPerCPU()->x86mmu.flushRVMappingAtomic(getReg(rs1), PAGE_SIZE);
			} else if (inst == 0x10200073) {
				this->handleSRET();
				this->handlePendingInterrupts();
				return;
			} else if (inst == 0x10500073) { // wfi
				this->pc += 4;

				// While no interrupt pending...
				while (this->stopi == 0) {
					asm volatile ("cli"); // Make this section atomic
					handlePendingInterrupts(); // Check if an interrupt happened meanwhile
					if (this->stopi == 0) { // If not, wait
						// Magic: sti is delayed, so any pending interrupt will break out of hlt.
						asm volatile("sti; hlt");
						handlePendingInterrupts();
					} else // It did -> resume execution
						asm volatile("sti");
				}

				return;
			} else
				panic("Unsupported misc instruction");

			break;
		}
		case 1u: // CSRRW
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			unsigned int rs1 = (inst >> 15u) & 31u;
			uint64_t rs1val = getReg(rs1);

			if (rd != 0u) // No getCSR side effect if rd is zero
				setReg(rd, getCSR(csr));

			setCSR(csr, rs1val);
			break;
		}
		case 2u: // CSRRS
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			unsigned int rs1 = (inst >> 15u) & 31u;
			uint64_t csrval = getCSR(csr);
			uint64_t rs1val = getReg(rs1);
			setReg(rd, getCSR(csr));

			if (rs1 != 0) // No setCSR side effect if rs1 is zero
				setCSR(csr, csrval | rs1val);

			break;
		}
		case 3u: // CSRRC
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			unsigned int rs1 = (inst >> 15u) & 31u;
			uint64_t csrval = getCSR(csr);
			uint64_t rs1val = getReg(rs1);
			setReg(rd, getCSR(csr));

			if (rs1 != 0) // No setCSR side effect if rs1 is zero
				setCSR(csr, csrval & ~rs1val);

			break;
		}
		case 5u: // CSRRWI
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			if (rd != 0u) // No getCSR side effect if rd is zero
				setReg(rd, getCSR(csr));

			uint64_t imm = (inst >> 15u) & 31u;
			setCSR(csr, imm);
			break;
		}
		case 6u: // CSRRSI
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			uint64_t imm = (inst >> 15u) & 31u;
			uint64_t csrval = getCSR(csr);
			setReg(rd, csrval);
			setCSR(csr, csrval | imm);
			break;
		}
		case 7u: // CSRRCI
		{
			uint16_t csr = inst >> 20u;
			unsigned int rd = (inst >> 7u) & 31u;
			uint64_t imm = (inst >> 15u) & 31u;
			uint64_t csrval = getCSR(csr);
			setReg(rd, csrval);
			setCSR(csr, csrval & ~imm);
			break;
		}
		default:
			panic("Unknown SYSTEM instruction");
		}

		// Immediately check for interrupts
		this->pc += 4;
		this->handlePendingInterrupts();
		return;
	}
	default:
		panic("Unknown instruction %08x", inst);
	}

	this->pc += 4;
}

void Hart::run()
{
	uint32_t counter = 0;
	for(;;)
	{
		if ((counter++ % 1024) == 0)
			this->handlePendingInterrupts();

		// Fetch 16 bits at a time. Due to IALIGN=16, a 32-bit wide instruction
		// may cross a page boundary and fault.
		uint16_t inst16;
		if (!this->fetchInstruction(&inst16, this->pc))
			continue;

		// 16-bit wide compressed instruction?
		if ((inst16 & 0b11) != 0b11)
		{
			runRVCInstruction(inst16);
			continue;
		}

		// (at least) 32-bit wide instruction. Fetch the remaining 16 bits.
		uint32_t inst = inst16;
		if (!this->fetchInstruction(&inst16, this->pc + 2))
			continue;

		inst |= inst16 << 16;
		runInstruction(inst);
	}
}
