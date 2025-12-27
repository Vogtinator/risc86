#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "x86jit.h"

/* How the JIT works:
 * In JIT generated code, %rdi points to the current struct Hart,
 * which is used to load/store registers and PC.
 * r8-r15 are dynamically allocated and are used for hart register state.
 * The PC is not updated for each instruction but only on demand.
 * The generated code returns with a status code in $eax, that is either
 * 0 on success or maps to scause in case of a fault.
 */

/* Ideas for further optimization:
 * - Relative jumps to the beginning of the current translation
 *   (loops) should stay inside the generated code
 * - Optimize findFreeDynReg by having an inverse map?
 * - Have the generated code push/pop clobbered dyn regs?
 * - Replace rvRegsToX86.inUse with generation counter?
 * - Only save/restore registers that are used by mappings?
 * - More special cases for reads from (0 immediate?) and writes to w0.
 * - Track immediate values in general and combine?
 */

void X86JIT::init()
{
	auto codeRegionPhys = physMemMgr.allocate(JIT_REGION_SIZE, MemRegionJIT);
	codeRegionStart = phys_to_virt<uint8_t>(codeRegionPhys);
	codeRegionEnd = codeRegionStart + JIT_REGION_SIZE;
	codeRegionCurrent = codeRegionStart;
}

bool X86JIT::tryJit(Hart *hart, PhysAddr pcPhys)
{
	// Not already translated?
	if (pcPhys != lastTranslationPCPhys) {
		// Make space for at least one translation
		if (codeRegionEnd - codeRegionCurrent < MIN_TRANSLATION_SPACE) {
			printf("JIT code region full, resetting.\n");
			reset();
		}

		// Try to make a translation
		uint8_t *newCodeStart = codeRegionCurrent;
		if (!translate(pcPhys))
			return false;

		lastTranslationPCPhys = pcPhys;
		lastTranslationCode = newCodeStart;
	}

	uint32_t scause = jumpToCode(hart, lastTranslationCode);

	if (scause != 0) { // Fault?
		// stval already set by the page fault handler
		hart->handleInterrupt(scause, hart->stval);
	}

	return true;
}

void X86JIT::reset()
{
	codeRegionCurrent = codeRegionStart;

	// Reset RV -> JIT code mappings
	lastTranslationPCPhys = 0;
	lastTranslationCode = nullptr;
}

uint32_t X86JIT::jumpToCode(Hart *hart, uint8_t *code)
{
	uint32_t ret;
	asm("call %A[code]"
	    : "=a" (ret)
	    : [code] "r" (code), "D" (hart)
	    : "memory", "cc",
	      "rcx", "rdx", "rbx",
	      "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15");

	return ret;
}

void X86JIT::emitREX(bool w, bool r, bool x, bool b)
{
	uint8_t rex = 0x40;
	if (w)
		rex |= 0x8;
	if (r)
		rex |= 0x4;
	if (x)
		rex |= 0x2;
	if (b)
		rex |= 0x1;

	emit8(rex);
}

void X86JIT::emitAddImmediate(X86Reg x86Reg, int32_t imm)
{
	if (imm >= INT8_MIN && imm <= INT8_MAX) {
		// add $imm8, %rvReg
		emitREX(true, false, false, regREXBit(x86Reg));
		emit8(0x83);
		emit8(0xC0 | regLow3Bits(x86Reg));
		emitRaw<int8_t>(imm);
		return;
	}

	// add $imm32, %rvReg
	emitREX(true, false, false, regREXBit(x86Reg));
	emit8(0x81);
	emit8(0xC0 | regLow3Bits(x86Reg));
	emitRaw<int32_t>(imm);
}

void X86JIT::emitMovRegReg(X86Reg from, X86Reg to)
{
	// mov %from, %to
	emitREX(true, regREXBit(from), false, regREXBit(to));
	emit8(0x89);
	emit8(0xC0 | (regLow3Bits(from) << 3) | regLow3Bits(to));
}

void X86JIT::emitXorRegReg(X86Reg x86Reg)
{
	// xor %x86reg, %x86reg
	// No need for 64bit operation, 32 bit clears upper half
	if (regREXBit(x86Reg))
		emitREX(false, regREXBit(x86Reg), false, regREXBit(x86Reg));

	emit8(0x31);
	emit8(0xC0 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(x86Reg));
}

void X86JIT::emitCliHlt()
{
	emit8(0xfa); // cli
	emit8(0xf4); // hlt
}

void X86JIT::emitLoadRVReg(RVReg rvReg, X86Reg x86Reg)
{
	if (rvReg == 0) {
		emitXorRegReg(x86Reg);
		return;
	}

	int32_t off = offsetof(Hart, regs[rvReg]);
	if (int8_t(off) == off) {
		// mov off8(%rdi), %x86reg
		emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
		emit8(0x8B);
		emit8(0x40 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
		emitRaw<int8_t>(off);
		return;
	}

	// mov off32(%rdi), %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x8B);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emitRaw<int32_t>(off);
}

void X86JIT::emitLoadPC(X86Reg x86Reg)
{
	// mov off32(%rdi), %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x8B);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emitRaw<int32_t>(offsetof(Hart, pc));
}

void X86JIT::emitStorePC(X86Reg x86Reg)
{
	// mov %x86reg, off32(%rdi)
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x89);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emitRaw<int32_t>(offsetof(Hart, pc));
}

void X86JIT::emitAddPC(int32_t value)
{
	if (int8_t(value) == value) {
		// addq $value8, off32(%rdi)
		emitREX(true, false, false, regREXBit(hartPtrReg));
		emit8(0x83);
		emit8(0x80 | regLow3Bits(hartPtrReg));
		emitRaw<int32_t>(offsetof(Hart, pc));
		emitRaw<int8_t>(value);
		return;
	}

	// addq $value32, off32(%rdi)
	emitREX(true, false, false, regREXBit(hartPtrReg));
	emit8(0x81);
	emit8(0x80 | regLow3Bits(hartPtrReg));
	emitRaw<int32_t>(offsetof(Hart, pc));
	emitRaw<int32_t>(value);
}

template<typename T>
void X86JIT::emitRaw(T value)
{
	if (codeRegionCurrent + sizeof(value) >= codeRegionEnd)
		panic("Attempted to write past code region end");

	memcpy(codeRegionCurrent, &value, sizeof(value));
	codeRegionCurrent += sizeof(value);
}

void X86JIT::emitStoreRVReg64(X86Reg x86Reg, RVReg rvReg)
{
	if (rvReg == 0)
		panic("Attempted to write to x0");

	int32_t off = offsetof(Hart, regs[rvReg]);
	if (int8_t(off) == off) {
		// mov %x86reg, off8(%rdi)
		emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
		emit8(0x89);
		emit8(0x40 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
		emitRaw<int8_t>(off);
		return;
	}

	// mov %x86reg, off32(%rdi)
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x89);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emitRaw<int32_t>(off);
}

void X86JIT::emitSExtX86Reg(X86Reg x86Reg)
{
	// movsxd %x86reg, %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(x86Reg));
	emit8(0x63);
	emit8(0xC0 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(x86Reg));
}

void X86JIT::emitRet(uint32_t retVal)
{
	if (retVal) {
		// mov $retVal, %eax
		emit8(0xB8);
		emitRaw<uint32_t>(retVal);
	} else
		emitXorRegReg(X86Reg::RAX);

	emit8(0xC3);
}

void X86JIT::emitFlushRVReg(RVReg rvReg)
{
	if (rvReg == 0)
		return;

	if (!rvRegsToX86[rvReg].dirty)
		return;

	if (rvRegsToX86[rvReg].bits32)
		emitSExtX86Reg(rvRegsToX86[rvReg].x86reg);

	emitStoreRVReg64(rvRegsToX86[rvReg].x86reg, rvReg);
}

void X86JIT::markRVRegFlushed(RVReg rvReg)
{
	rvRegsToX86[rvReg].dirty = false;
	rvRegsToX86[rvReg].bits32 = false; // Got sign extended as side effect
}

X86JIT::X86Reg X86JIT::findFreeDynReg()
{
	// Try to find a free register
	for (X86Reg r = x86DynRegFirst; r < x86DynRegLast; r = X86Reg(uint8_t(r) + 1)) {
		bool mapped = false;
		for (int rv = 0; rv < 32; ++rv) {
			if (rvRegsToX86[rv].x86reg == r) {
				mapped = true;
				break;
			}
		}

		if (!mapped)
			return r;
	}

	// Nothing found - flush the first mapping not used by this instruction
	for (int rv = 0; rv < 32; ++rv) {
		if (rvRegsToX86[rv].x86reg == NotMapped
		    || rvRegsToX86[rv].inUse)
			continue;

		auto ret = rvRegsToX86[rv].x86reg;
		emitFlushRVReg(rv);
		markRVRegFlushed(rv);
		rvRegsToX86[rv].x86reg = NotMapped;
		return ret;
	}

	panic("All dynamic regs in use by one instruction?");
}

X86JIT::X86Reg X86JIT::mapRVRegForRead(RVReg rvReg, bool bits32Ok)
{
	auto &mapEntry = rvRegsToX86[rvReg];
	if (mapEntry.x86reg == NotMapped) {
		mapEntry.x86reg = findFreeDynReg();
		emitLoadRVReg(rvReg, mapEntry.x86reg);
	} else if (rvReg == 0) {
		// Reading from x0 is always 0.
		if (mapEntry.dirty)
			emitLoadRVReg(0, mapEntry.x86reg);

		mapEntry.dirty = false;
		mapEntry.bits32 = false;
	} else if (mapEntry.bits32 && !bits32Ok) {
		emitSExtX86Reg(rvRegsToX86[rvReg].x86reg);
		mapEntry.bits32 = false;
	}

	mapEntry.inUse = true;
	return mapEntry.x86reg;
}

X86JIT::X86Reg X86JIT::mapRVRegForRead64(RVReg rvReg)
{
	return mapRVRegForRead(rvReg, false);
}

X86JIT::X86Reg X86JIT::mapRVRegForRead32(RVReg rvReg)
{
	return mapRVRegForRead(rvReg, true);
}

X86JIT::X86Reg X86JIT::mapRVRegForWrite64(RVReg rvReg)
{
	auto &mapEntry = rvRegsToX86[rvReg];
	if (mapEntry.x86reg == NotMapped) {
		mapEntry.x86reg = findFreeDynReg();
	}

	mapEntry.dirty = true;
	mapEntry.inUse = true;
	return mapEntry.x86reg;
}

X86JIT::X86Reg X86JIT::mapRVRegForWrite32(RVReg rvReg)
{
	auto &mapEntry = rvRegsToX86[rvReg];
	if (mapEntry.x86reg == NotMapped) {
		mapEntry.x86reg = findFreeDynReg();
	}

	mapEntry.bits32 = true;
	mapEntry.dirty = true;
	mapEntry.inUse = true;
	return mapEntry.x86reg;
}

void X86JIT::emitFlushRegsToHart()
{
	for (int rv = 0; rv < 32; ++rv)
		emitFlushRVReg(rv);
}

void X86JIT::emitUpdateHartPC(PhysAddr curPC)
{
	if (lastHartPC == curPC)
		return;

	if (lastHartPC >= curPC)
		panic("Negative PC offset in translation?");

	emitAddPC(curPC - lastHartPC);
	lastHartPC = curPC;
}

bool X86JIT::translateRVCInstruction(PhysAddr addr, uint16_t inst)
{
	return false;
}

bool X86JIT::translateInstruction(PhysAddr addr, uint32_t inst)
{
	uint32_t opc = inst & 0x7Fu;
	switch(opc) {
	case 0x03u: // load
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rd = (inst >> 7u) & 31u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		int32_t imm = int32_t(inst) >> 20u;

		if (funct3 > 6)
			return false;

		// %rdx = rs1 + imm
		X86Reg rs1X86 = mapRVRegForRead64(rs1);
		emitMovRegReg(rs1X86, X86Reg::RDX);
		emitAddImmediate(X86Reg::RDX, imm);

		// clc
		emit8(0xf8);

		switch (funct3)
		{
		case 0u: // lb
		case 4u: // lbu
			// mov (%rdx), %al
			emit8(0x8A); emit8(0x02);
			break;
		case 1u: // lh
		case 5u: // lhu
			// mov (%rdx), %ax
			emit8(0x66); emit8(0x8B); emit8(0x02);
			break;
		case 2u: // lw
		case 6u: // lwu
			// mov (%rdx), %eax
			emit8(0x8B); emit8(0x02);
			break;
		case 3u: // ld
			// mov (%rdx), %rax
			emit8(0x48); emit8(0x8B); emit8(0x02);
			break;
		default:
			panic("Unknown load instruction");
		}

		// If no fault (carry clear), skip fault handling
		emit8(0x73); // jnc off8
		uint8_t *jmpOffPtr = codeRegionCurrent;
		emit8(0); // off8 for jmp, will be adjusted below

		// Fault handling: Flush regs and return
		// Update PC and relative jmp in one go
		emitAddPC(addr - lastHartPC);
		// Leave translation
		emitFlushRegsToHart();
		emitRet(Hart::SCAUSE_LOAD_PAGE_FAULT);

		int jmpOff = codeRegionCurrent - jmpOffPtr - 1;
		if (jmpOff < 0 || jmpOff >= INT8_MAX)
			panic("jmp offset too big");

		*jmpOffPtr = int8_t(jmpOff);

		// No fault: Store result in rd
		X86Reg rdX86 = mapRVRegForWrite64(rd);
		switch (funct3)
		{
		case 0u: // lb
			// movsx %al, %rdX86
			emitREX(true, regREXBit(rdX86), false, false);
			emit8(0x0F); emit8(0xBE);
			emit8(0xC0 | (regLow3Bits(rdX86) << 3));
			break;
		case 4u: // lbu
			// movzx %al, %rdX86
			emitREX(true, regREXBit(rdX86), false, false);
			emit8(0x0F); emit8(0xB6);
			emit8(0xC0 | (regLow3Bits(rdX86) << 3));
			break;
		case 1u: // lh
			// movsx %ax, %rdX86
			emitREX(true, regREXBit(rdX86), false, false);
			emit8(0x0F); emit8(0xBF);
			emit8(0xC0 | (regLow3Bits(rdX86) << 3));
			break;
		case 5u: // lhu
			// movzx %ax, %rdX86
			emitREX(true, regREXBit(rdX86), false, false);
			emit8(0x0F); emit8(0xB7);
			emit8(0xC0 | (regLow3Bits(rdX86) << 3));
			break;
		case 2u: // lw
			// movsx %eax, %rdX86
			emitREX(true, regREXBit(rdX86), false, false);
			emit8(0x63);
			emit8(0xC0 | (regLow3Bits(rdX86) << 3));
			break;
		case 6u: // lwu
		case 3u: // ld
			// No sign extension needed
			emitMovRegReg(X86Reg::RAX, rdX86);
			break;
		default:
			panic("Unknown load instruction");
		}

		return true;
	}
	case 0x17u: // auipc
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		int32_t imm = inst & 0xFFFFF000u;

		// %rvReg = current PC
		emitUpdateHartPC(addr);
		X86Reg rdX86 = mapRVRegForWrite64(rd);
		emitLoadPC(rdX86);

		// add $imm, %rvReg
		emitAddImmediate(rdX86, imm);
		return true;
	}
	case 0x63u: // branch
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		uint32_t imm12 = inst >> 31u;
		uint32_t imm105 = (inst >> 25u) & 0x3fu;
		uint32_t imm41 = (inst >> 8u) & 0xfu;
		uint32_t imm11 = (inst >> 7u) & 0x1u;
		int32_t imm = int32_t(((imm12 << 12u) | (imm11 << 11u) | (imm105 << 5u) | (imm41 << 1u)) << 19u) >> 19u;

		X86Reg rs1X86 = mapRVRegForRead64(rs1),
		       rs2X86 = mapRVRegForRead64(rs2);

		// cmp %rs2X86, %rs1X86
		emitREX(true, regREXBit(rs2X86), false, regREXBit(rs1X86));
		emit8(0x39);
		emit8(0xC0 | (regLow3Bits(rs2X86) << 3) | regLow3Bits(rs1X86));

		// If comparison false, skip the jump away
		switch (funct3)
		{
		case 0u: // beq
			emit8(0x75); // jne off8
			break;
		case 1u: // bne
			emit8(0x74); // je off8
			break;
		case 4u: // blt
			emit8(0x7D); // jge off8
			break;
		case 5u: // bge
			emit8(0x7C); // jl off8
			break;
		case 6u: // bltu
			emit8(0x73); // jae off8
			break;
		case 7u: // bgeu
			emit8(0x72); // jb off8
			break;
		default:
			panic("Unsupported branch");
		}

		uint8_t *jmpOffPtr = codeRegionCurrent;
		emit8(0); // off8 for jmp, will be adjusted below

		// Update PC and relative jmp in one go
		emitAddPC(addr - lastHartPC + imm);
		// Leave translation
		emitFlushRegsToHart();
		emitRet(0);

		int jmpOff = codeRegionCurrent - jmpOffPtr - 1;
		if (jmpOff < 0 || jmpOff >= INT8_MAX)
			panic("jmp offset too big");

		*jmpOffPtr = int8_t(jmpOff);

		return true;
	}
	case 0x67u: // jalr
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		uint32_t rs1 = (inst >> 15u) & 31u;
		int32_t imm = int32_t(inst) >> 20u;

		// Prepare return address in %rax.
		// %rax = current PC
		emitUpdateHartPC(addr);

		if (rd != 0) {
			emitLoadPC(X86Reg::RAX);
			// add $4, %rax
			emitAddImmediate(X86Reg::RAX, 4);
		}

		// hart->pc = getReg(rs1)
		X86Reg rs1X86 = mapRVRegForRead64(rs1);
		emitStorePC(rs1X86);
		// hart->pc += imm;
		emitAddPC(imm);

		jumpsAway = true;

		// Write return address in %rax to rd.
		if (rd != 0) {
			X86Reg rdX86 = mapRVRegForWrite64(rd);
			emitMovRegReg(X86Reg::RAX, rdX86);
		}
		return true;
	}
	case 0x6fu: // jal
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		uint32_t imm20 = inst >> 31u;
		uint32_t imm101 = (inst >> 21u) & 0x3FFu;
		uint32_t imm11 = (inst >> 20u) & 1u;
		uint32_t imm1912 = (inst >> 12u) & 0xFFu;
		int32_t imm = int32_t(((imm20 << 20u) | (imm1912 << 12u) | (imm11 << 11u) | (imm101 << 1u)) << 11u) >> 11u;

		emitUpdateHartPC(addr);

		// Return address into rd
		if (rd != 0) {
			X86Reg rdX86 = mapRVRegForWrite64(rd);
			emitLoadPC(rdX86);
			emitAddImmediate(rdX86, 4);
		}

		emitAddPC(imm);

		jumpsAway = true;
		return true;
	}
	default:
		// printf("Unhandled instruction %08x", inst);
	}

	return false;
}

bool X86JIT::translate(PhysAddr entry)
{
	PhysAddr addr = entry;

	// Fresh translation, reset state.

	// Reset register mappings
	for (int i = 0; i < 32; ++i)
		rvRegsToX86[i] = {};

	lastHartPC = entry;
	jumpsAway = false;

	for (;;)
	{
		if (jumpsAway)
			break;

		if ((addr & 0xFFF) > 0xFFE)
			break; // Can't cross a page boundary.

		if (codeRegionEnd - codeRegionCurrent < MIN_TRANSLATION_SPACE)
			break;

		// No regs in use by the next instruction yet
		for (int i = 0; i < 32; ++i)
			rvRegsToX86[i].inUse = false;

		uint16_t inst16 = *phys_to_virt<uint16_t>(addr);
		if ((inst16 & 0b11) == 0b11) { // 32bit instruction?
			if ((addr & 0xFFF) > 0xFFC)
				break; // Can't cross a page boundary.

			uint32_t inst32 = *phys_to_virt<uint32_t>(addr);
			if (!translateInstruction(addr, inst32))
				break;

			addr += 4;
			continue;
		}

		if (!translateRVCInstruction(addr, inst16))
			break;

		addr += 2;
	}

	// Did not translate any instructions
	if (addr == entry)
		return false;

	// Save new PC if the last instruction doesn't set it.
	if (!jumpsAway)
		emitUpdateHartPC(addr);

	emitFlushRegsToHart();
	emitRet(0);

	return true;
}
