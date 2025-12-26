#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "x86jit.h"

/* How the JIT works:
 * In JIT generated code, %rdi points to the current struct Hart,
 * which is used to load/store registers and PC.
 * r8-r15 are dynamically allocated and are used for hart register state.
 * The PC is not updated for each instruction but only on demand
 */

/* Ideas for further optimization:
 * - Optimize findFreeDynReg by having an inverse map?
 * - Only save/restore registers that are used by mappings?
 * - More special cases for reads from (0 immediate?) and writes to w0.
 * - Track immediate values in general and combine?
 */

void X86JIT::init()
{
	auto codeRegionPhys = physMemMgr.allocate(JIT_REGION_SIZE, MemRegionJIT);
	codeRegionStart = phys_to_virt<uint8_t>(codeRegionPhys);
	codeRegionEnd = codeRegionStart + JIT_REGION_SIZE;
}

bool X86JIT::tryJit(Hart *hart, PhysAddr pcPhys)
{
	// Not already translated?
	if (pcPhys != lastTranslationPCPhys) {
		// Try to make a translation
		auto newCodeStart = codeRegionCurrent;
		if (!translate(pcPhys))
			return false;

		lastTranslationPCPhys = pcPhys;
		lastTranslationCode = newCodeStart;
	}

	jumpToCode(hart, lastTranslationCode);
	return true;
}

void X86JIT::reset()
{
	codeRegionCurrent = codeRegionStart;

	// Reset RV -> JIT code mappings
	lastTranslationPCPhys = 0;
	lastTranslationCode = nullptr;
}

void X86JIT::jumpToCode(Hart *hart, uint8_t *code)
{
	asm("call %A[code]" :: [code] "r" (code), "D" (hart)
	    : "memory", "cc",
	      "rax", "rcx", "rdx", "rbx",
	      "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15");
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

void X86JIT::emitLoadRVReg(RVReg rvReg, X86Reg x86Reg)
{
	if (rvReg == 0) {
		// xor %x86reg, %x86reg
		// No need for 64bit operation, 32 bit clears upper half
		if (regREXBit(x86Reg))
			emitREX(false, regREXBit(x86Reg), false, regREXBit(x86Reg));

		emit8(0x31);
		emit8(0xC0 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(x86Reg));
		return;
	}

	// mov off8(%rdi), %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x8B);
	emit8(0x40 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emit8(offsetof(Hart, regs[rvReg]));
}

void X86JIT::emitLoadPC(X86Reg x86Reg)
{
	// mov off32(%rdi), %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x8B);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emit<uint32_t>(offsetof(Hart, pc));
}

void X86JIT::emitStorePC(X86Reg x86Reg)
{
	// mov %x86reg, off32(%rdi)
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x89);
	emit8(0x80 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emit<uint32_t>(offsetof(Hart, pc));
}

void X86JIT::emitAddPC(int32_t value)
{
	if (int8_t(value) == value) {
		// addq $value8, off32(%rdi)
		emitREX(true, false, false, regREXBit(hartPtrReg));
		emit8(0x83);
		emit8(0x80 | regLow3Bits(hartPtrReg));
		emit<uint32_t>(offsetof(Hart, pc));
		emit<int8_t>(value);
		return;
	}

	// addq $value32, off32(%rdi)
	emitREX(true, false, false, regREXBit(hartPtrReg));
	emit8(0x81);
	emit8(0x80 | regLow3Bits(hartPtrReg));
	emit<uint32_t>(offsetof(Hart, pc));
	emit<int32_t>(value);
}

template<typename T>
void X86JIT::emit(T value)
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

	// mov %x86reg, off(%rdi)
	emitREX(true, regREXBit(x86Reg), false, regREXBit(hartPtrReg));
	emit8(0x89);
	emit8(0x40 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(hartPtrReg));
	emit8(offsetof(Hart, regs[rvReg]));
}

void X86JIT::emitSExtX86Reg(X86Reg x86Reg)
{
	// movsxd %x86reg, %x86reg
	emitREX(true, regREXBit(x86Reg), false, regREXBit(x86Reg));
	emit8(0x63);
	emit8(0xC0 | (regLow3Bits(x86Reg) << 3) | regLow3Bits(x86Reg));
}

void X86JIT::emitRet()
{
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

	// Nothing found - flush the first not used one
	for (int rv = 0; rv < 32; ++rv) {
		if (rvRegsToX86[rv].inUse)
			continue;

		auto ret = rvRegsToX86[rv].x86reg;
		emitFlushRVReg(rv);
		markRVRegFlushed(rv);
		rvRegsToX86[rv].x86reg = NotMapped;
		return ret;
	}

	panic("All dynamic regs in use by one instruction?");
}

X86JIT::X86Reg X86JIT::mapRVRegForRead64(RVReg rvReg)
{
	auto &mapEntry = rvRegsToX86[rvReg];
	if (mapEntry.x86reg == NotMapped) {
		mapEntry.x86reg = findFreeDynReg();
		emitLoadRVReg(rvReg, mapEntry.x86reg);
	} else if (mapEntry.bits32) {
		emitSExtX86Reg(rvRegsToX86[rvReg].x86reg);
		mapEntry.bits32 = false;
	}

	mapEntry.inUse = true;
	return mapEntry.x86reg;
}

X86JIT::X86Reg X86JIT::mapRVRegForRead32(RVReg rvReg)
{
	auto &mapEntry = rvRegsToX86[rvReg];
	if (mapEntry.x86reg == NotMapped) {
		mapEntry.x86reg = findFreeDynReg();
		emitLoadRVReg(rvReg, mapEntry.x86reg);
	}

	mapEntry.inUse = true;
	return mapEntry.x86reg;
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
}

bool X86JIT::translateRVCInstruction(PhysAddr addr, uint16_t inst)
{
	return false;
}

bool X86JIT::translateInstruction(PhysAddr addr, uint32_t inst)
{
	return false;
}

bool X86JIT::translate(PhysAddr entry)
{
	PhysAddr addr = entry;

	if (codeRegionEnd - codeRegionStart < 64) {
		printf("JIT code region full, resetting.\n");
		reset();
	}

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

		if (codeRegionEnd - codeRegionStart < 64)
			break;

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

	// Generate epilog if the last instruction did not already
	// do it for us.
	if (!jumpsAway) {
		emitFlushRegsToHart();
		emitUpdateHartPC(addr);
		emitRet();
	}

	return true;
}
