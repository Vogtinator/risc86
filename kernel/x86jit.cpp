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
 * - Make emitAddImmediate with 0 a no-op
 * - Optimize findFreeDynReg by having an inverse map?
 * - Have the generated code push/pop clobbered dyn regs?
 * - Replace rvRegsToX86.inUse with generation counter?
 * - Only save/restore registers that are used by mappings?
 * - More special cases for reads from (0 immediate?) and writes to w0.
 *   (esp. jal?)
 * - Track immediate values in general and combine?
 * - Pad new translations so that that don't invalidate old ones in the i$?
 * - More eager flushing of dirty regs also in the hot path,
 *   so that there's less flushing in early exit (cond jump, fault handle) paths?
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

void X86JIT::emitMovImmediate32(X86Reg x86Reg, int32_t imm)
{
	// mov $imm32, %x86Reg
	emitREX(false, false, false, regREXBit(x86Reg));
	emit8(0xB8 | regLow3Bits(x86Reg));
	emitRaw<int32_t>(imm);
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
	if (value == 0)
		return;

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

	// TODO: Why does clang not pick the builtin here when using plain memcpy?
	__builtin_memcpy(codeRegionCurrent, &value, sizeof(value));

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
		if (r == X86Reg::R12) {
			// Its three low bits are the same as %rsp, so %r12 also gets special
			// treatment in ModRM. Just avoid it.
			continue;
		}

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

	mapEntry.bits32 = false;
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

X86JIT::X86Reg X86JIT::mapRVRegForReadWrite64(RVReg rvReg)
{
	X86Reg ret = mapRVRegForRead64(rvReg);
	(void) mapRVRegForWrite64(rvReg);
	return ret;
}

void X86JIT::emitFlushRegsToHart()
{
	for (int rv = 0; rv < 32; ++rv)
		emitFlushRVReg(rv);
}

void X86JIT::emitPCRelativeJump(PhysAddr pcPhys, int32_t imm)
{
	// Also include the hart->pc update
	int64_t imm64 = pcPhys - lastHartPC + imm;
	if (imm64 < INT32_MIN || imm64 > INT32_MAX)
		panic("Offset does not fit");

	emitAddPC(imm64);
	emitFlushRegsToHart();

	// Calculate the new PC
	auto newPcPhys = pcPhys + imm;

	// If it's not the beginning of this translation, just exit
	if (newPcPhys != thisTranslationStartPC) {
		emitRet(0);
		return;
	}

	// Otherwise, loop back to the start!
	int32_t jmpOff = thisTranslationStartCode - codeRegionCurrent;
	// TODO: ret in case there's an interrupt

	// Short jump?
	int32_t jmpOffShort = jmpOff - 2;
	if (jmpOffShort >= INT8_MIN && jmpOffShort <= INT8_MAX) {
		// jmp off8
		emit8(0xEB);
		emitRaw<int8_t>(jmpOffShort);
		return;
	}

	int32_t jmpOffNear = jmpOff - 5;
	// jmp off32
	emit8(0xE9);
	emitRaw<int32_t>(jmpOffNear);
}

void X86JIT::emitLeaveOnMemFault(PhysAddr curPC, uint32_t scause)
{
	// If no fault (carry clear), skip fault handling
	emit8(0x73); // jnc off8
	uint8_t *jmpOffPtr = codeRegionCurrent;
	emit8(0); // off8 for jmp, will be adjusted below

	// Fault handling: Flush regs and return
	emitAddPC(curPC - lastHartPC);
	// Leave translation
	emitFlushRegsToHart();
	emitRet(scause);

	int jmpOff = codeRegionCurrent - jmpOffPtr - 1;
	if (jmpOff < INT8_MIN || jmpOff >= INT8_MAX)
		panic("jmp offset too big");

	*jmpOffPtr = int8_t(jmpOff);
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

		emitPCRelativeJump(addr, offs);
		return true;
	} else if ((inst & 0xE003) == 0x4001) { // c.li
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >> 2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = (inst >> 7) & 0x1F;

		X86Reg rdX86 = mapRVRegForWrite32(rd);
		emitMovImmediate32(rdX86, imm);
		return true;
	} else if ((inst & 0b111'0'11111'00000'11) == 0b000'0'00000'00000'01) { // c.nop (before c.addi)
		// nop
		return true;
	} else if ((inst & 0b111'0'00000'00000'11) == 0b000'0'00000'00000'01) { // c.addi (after c.nop)
		uint16_t imm5  = (inst >> 12) & 1,
		        imm40 = (inst >>  2) & 0x1F;

		int16_t imm = int16_t(((imm5 << 5) | imm40) << 10) >> 10;
		uint32_t rd = (inst >> 7) & 0x1F;

		X86Reg rdX86 = mapRVRegForReadWrite64(rd);
		emitAddImmediate(rdX86, imm);
		return true;
	} else if ((inst & 0b111'000000'00000'11) == 0b111'000000'00000'10) { // c.sdsp
		uint16_t imm53 = (inst >> 10) & 7,
		        imm86 = (inst >>  7) & 7;

		uint16_t off = (imm86 << 6) | (imm53 << 3);

		uint32_t rs2 = (inst >> 2) & 31;

		// %rdx = x2 + imm
		X86Reg spX86 = mapRVRegForRead64(2);
		emitMovRegReg(spX86, X86Reg::RDX);
		emitAddImmediate(X86Reg::RDX, off);

		// %rax = rs2
		X86Reg rs2X86 = mapRVRegForRead64(rs2);
		emitMovRegReg(rs2X86, X86Reg::RAX);

		// clc
		emit8(0xf8);

		// mov %rax, (%rdx)
		emit8(0x48); emit8(0x89); emit8(0x02);

		emitLeaveOnMemFault(addr, Hart::SCAUSE_STORE_PAGE_FAULT);
		return true;
	}  else if ((inst & 0b111'0'00000'00000'11) == 0b011'0'00000'00000'10) { // c.ldsp
		uint16_t imm5  = (inst >> 12) & 1,
		        imm43 = (inst >>  5) & 3,
		        imm86 = (inst >>  2) & 7;

		uint16_t off = (imm86 << 6) | (imm5 << 5) | (imm43 << 3);

		uint32_t rd = (inst >> 7) & 31;

		if (rd == 0)
			panic("Reserved instruction %x", inst);

		// %rdx = x2 + imm
		X86Reg spX86 = mapRVRegForRead64(2);
		emitMovRegReg(spX86, X86Reg::RDX);
		emitAddImmediate(X86Reg::RDX, off);

		// clc
		emit8(0xf8);

		// mov (%rdx), %rax
		emit8(0x48); emit8(0x8B); emit8(0x02);

		emitLeaveOnMemFault(addr, Hart::SCAUSE_LOAD_PAGE_FAULT);

		X86Reg rdX86 = mapRVRegForWrite64(rd);
		emitMovRegReg(X86Reg::RAX, rdX86);
		return true;
	} else if ((inst & 0b111'1'00000'11111'11) == 0b100'0'00000'00000'10) { // c.jr (before c.mv)
		uint32_t rs1 = (inst >> 7) & 0x1F;

		if (rs1 == 0)
			panic("Reserved c.???");

		// hart->pc = getReg(rs1)
		X86Reg rs1X86 = mapRVRegForRead64(rs1);
		emitStorePC(rs1X86);

		jumpsAway = true;
		emitFlushRegsToHart();
		emitRet(0);
		return true;
	} else if ((inst & 0b111'1'00000'00000'11) == 0b100'0'00000'00000'10) { // c.mv (after c.jr)
		uint32_t rs2 = (inst >> 2) & 0x1F,
		        rd  = (inst >> 7) & 0x1F;

		X86Reg rs2X86 = mapRVRegForRead64(rs2),
		       rdX86 = mapRVRegForWrite64(rd);

		emitMovRegReg(rs2X86, rdX86);
		return true;
	}

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

		emitLeaveOnMemFault(addr, Hart::SCAUSE_LOAD_PAGE_FAULT);

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
	case 0x23u: // store
	{
		uint32_t funct3 = (inst >> 12u) & 7u;
		uint32_t rs1 = (inst >> 15u) & 31u;
		uint32_t rs2 = (inst >> 20u) & 31u;
		int32_t imm = ((int32_t(inst) >> 25u) << 5u) | ((inst >> 7u) & 0x1Fu);

		if (funct3 > 3)
			return false;

		// %rdx = rs1 + imm
		X86Reg rs1X86 = mapRVRegForRead64(rs1);
		emitMovRegReg(rs1X86, X86Reg::RDX);
		emitAddImmediate(X86Reg::RDX, imm);

		// %rax = rs2
		X86Reg rs2X86;
		if (funct3 == 3)
			rs2X86 = mapRVRegForRead64(rs2);
		else
			rs2X86 = mapRVRegForRead32(rs2);

		emitMovRegReg(rs2X86, X86Reg::RAX);

		// clc
		emit8(0xf8);

		switch (funct3)
		{
		case 0u: // sb
			// mov %al, (%rdx)
			emit8(0x88); emit8(0x02);
			break;
		case 1u: // sh
			// mov %ax, (%rdx)
			emit8(0x66); emit8(0x89); emit8(0x02);
			break;
		case 2u: // sw
			// mov %eax, (%rdx)
			emit8(0x89); emit8(0x02);
			break;
		case 3u: // sd
			// mov %rax, (%rdx)
			emit8(0x48); emit8(0x89); emit8(0x02);
			break;
		default:
			panic("Unknown store");
		}

		emitLeaveOnMemFault(addr, Hart::SCAUSE_STORE_PAGE_FAULT);
		return true;
	}
	case 0x37u: // lui
	{
		uint32_t rd = (inst >> 7u) & 0x1Fu;
		int32_t imm = int32_t(inst & 0xFFFFF000u);

		X86Reg rdX86 = mapRVRegForWrite32(rd);
		emitMovImmediate32(rdX86, imm);
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

		emitPCRelativeJump(addr, imm);

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

		emitFlushRegsToHart();
		emitRet(0);
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

		emitPCRelativeJump(addr, imm);

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

	thisTranslationStartPC = entry;
	thisTranslationStartCode = codeRegionCurrent;
	lastHartPC = entry;
	jumpsAway = false;

	uint8_t *lastInstructionEnd;

	for (;;)
	{
		lastInstructionEnd = codeRegionCurrent;

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

	// Partially translated instructions can mess up reg mapping
	if (lastInstructionEnd != codeRegionCurrent)
		panic("Failed instruction translation for %lx still emitted code!", addr);

	// Did not translate any instructions
	if (addr == entry)
		return false;

	// Leave the generated code if the last translation didn't do that already.
	if (!jumpsAway) {
		emitUpdateHartPC(addr);
		emitFlushRegsToHart();
		emitRet(0);
	}

	return true;
}
