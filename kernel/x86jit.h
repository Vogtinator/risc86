#pragma once

#include "hart.h"
#include "mem.h"
#include "x86interrupts.h"

class X86JIT
{
public:
	X86JIT();
	// Allocate memory for the generated code
	void init();

	// Try to generate (if necessary) and run code for the current hart state.
	// Returns false if fallback to the interpreter is neccessary.
	bool tryJit(Hart *hart, PhysAddr pcPhys);
	bool tryJitVirt(Hart *hart, uint64_t pcVirt);
	void resetVirtMap();

	// Discard all translations.
	void reset();

	// Called by X86MMU on page faults.
	CALLED_FROM_IRQ
	bool handlePageFault(Hart *hart, struct InterruptFrame *frame, bool isWrite);
private:
	const size_t JIT_REGION_SIZE = 64*1024*1024; // 64 MiB
	const int MIN_TRANSLATION_SPACE = 256;

	__attribute__((warn_unused_result))
	uint32_t jumpToCode(Hart *hart, uint8_t *code);

	// Region where the generated code is stored and executed from
	uint8_t *codeRegionStart, *codeRegionEnd;
	// Watermark between used and free parts of the code region.
	// The next instruction will be written here.
	uint8_t *codeRegionCurrent;

	// Append value (with memcpy) to the current code region pointer
	// and advance it.
	template <typename T> void emitRaw(T value);
	inline void emit8(uint8_t value) { emitRaw(value); }

	// Helpers for emitting x86 instructions
	enum class X86Reg {
		RAX=0, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
		R8, R9, R10, R11, R12, R13, R14, R15,
	};
	static inline constexpr bool regREXBit(X86Reg r) { return static_cast<uint8_t>(r) & 0b1000; }
	static inline uint8_t regLow3Bits(X86Reg r) { return static_cast<uint8_t>(r) & 0b0111; }

	enum class XMMReg {
		XMM0=0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
		XMM8=8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
	};
	static inline constexpr bool regREXBit(XMMReg r) { return static_cast<uint8_t>(r) & 0b1000; }
	static inline uint8_t regLow3Bits(XMMReg r) { return static_cast<uint8_t>(r) & 0b0111; }

	using RVReg = uint8_t;

	void emitREX(bool w, bool r, bool x, bool b);
	void emitModRMMem(uint8_t reg, uint8_t base, int32_t disp);
	// Zero-extended to the 64bit register
	void emitMovImmediate32(X86Reg x86Reg, uint32_t imm);
	// Sign-extended to the 64bit register
	void emitMovImmediate64(X86Reg x86Reg, int32_t imm);
	void emitAddImmediate(X86Reg x86Reg, int32_t imm);
	void emitMovRegReg(X86Reg from, X86Reg to);
	void emitMovRegReg(XMMReg from, XMMReg to, bool isDouble);
	void emitMovRegReg(XMMReg from, X86Reg to, bool isDouble);
	void emitMovRegReg(X86Reg from, XMMReg to, bool isDouble);
	void emitXorRegReg(X86Reg x86Reg);
	void emitCliHlt(); // For debugging
	void emitMovMem(X86Reg base, int32_t disp, X86Reg data, bool isLoad, uint8_t size);
	void emitMovMemXMM(X86Reg base, int32_t disp, XMMReg data, bool isLoad, uint8_t size);

	enum VEXOpcPrefix {
		VEX_0F=1, VEX_0F_38=2, VEX_0F_3A=3,
	};
	enum VEXSIMDPrefix {
		VEX_NONE=0, VEX_66, VEX_F3, VEX_F2,
	};

	static const XMMReg VEXNoV = static_cast<XMMReg>(0);

	// Implied V=0 (scalar or 128bit)
	void emitVEX(bool w, VEXOpcPrefix m, VEXSIMDPrefix pp, bool r, bool x, bool b, XMMReg v);

	// Low-level helpers for RV register management
	static const X86Reg hartPtrReg = X86Reg::RDI, hartPCReg = X86Reg::R12;
	static const size_t hartPtrBias = offsetof(Hart, regs[16]);
	void emitLoadRVReg(RVReg rvReg, X86Reg x86Reg);
	void emitLoadPC(X86Reg x86Reg);
	void emitStorePC(X86Reg x86Reg);
	void emitAddPC(int32_t value);
	void emitStoreRVReg64(X86Reg x86Reg, RVReg rvReg);
	void emitSExtX86Reg(X86Reg x86Reg); // 32->64 sign extension
	void emitRet();

	// High-level helpers for RV register management
	// Use %r8-%r15, but skip %r12 as it has a different meaning in ModRM...
	static const X86Reg x86DynRegFirst = X86Reg::R8, x86DynRegLast = X86Reg::R15;
	// Flushes RV reg to struct Hart, does not change reg map.
	void emitFlushRVReg(RVReg rvReg);
	// Changes reg map according to what emitFlushRVReg did.
	void markRVRegFlushed(RVReg rvReg);
	X86Reg findFreeDynReg();
	// Make rvReg accessible as a x86 register for reading.
	// Set bits32Ok if only the low 32 bits are used.
	X86Reg mapRVRegForRead(RVReg rvReg, bool bits32Ok);
	X86Reg mapRVRegForRead64(RVReg rvReg);
	X86Reg mapRVRegForRead32(RVReg rvReg);
	// Like mapRVRegForRead, but for writing.
	// If is32bits is set, the value will be sign-extended from 32bit to 64bit when needed.
	// Note: From this point on it assumes that the x86 register has valid contents,
	// so make sure that if the register is read, mapRVRegForRead is called before.
	// Rule of thumb: Call mapRVRegForRead before any mapRVRegForWrite.
	// mapRVRegForReadWrite* does that for you.
	X86Reg mapRVRegForWrite(RVReg rvReg, bool is32bits);
	X86Reg mapRVRegForWrite32(RVReg rvReg);
	X86Reg mapRVRegForWrite64(RVReg rvReg);
	X86Reg mapRVRegForReadWrite64(RVReg rvReg);
	X86Reg mapRVRegForReadWrite32(RVReg rvReg);

	// Same for FP regs
	// Low-level
	void emitLoadRVFReg(RVReg rvReg, XMMReg xmmReg);
	void emitStoreRVFReg64(XMMReg xmmReg, RVReg rvReg);
	void emitNANBoxXMMReg(XMMReg xmmReg); // Set high 32bits to 0xFFFFFFFF
	// High-level
	// TODO: What's the right number?
	static const XMMReg xmmDynRegFirst = XMMReg::XMM2, xmmDynRegLast = XMMReg::XMM11;
	static const XMMReg xmmNANBoxReg = XMMReg::XMM1;
	// Flushes RV FP reg to struct Hart, does not change reg map.
	void emitFlushRVFReg(RVReg rvReg);
	// Changes reg map accordingly.
	void markRVFRegFlushed(RVReg rvReg);
	XMMReg findFreeXMMDynReg();
	// Make rvReg accessible as a XMM register for reading.
	// Set bits32Ok if only the low 32 bits are used.
	XMMReg mapRVFRegForRead(RVReg rvReg, bool bits32Ok);
	// Like mapRVRegForRead, but for writing.
	// Note: From this point on it assumes that the x86 register has valid contents,
	// so make sure that if the register is read, mapRVRegForRead is called before.
	// Rule of thumb: Call mapRVRegForRead before any mapRVRegForWrite.
	// mapRVRegForReadWrite* does that for you.
	XMMReg mapRVFRegForWrite(RVReg rvReg, bool is32bits);

	// Flush all mapped registers marked dirty back into Hart::(f)regs, does not chage reg map.
	void emitFlushRegsToHart();
	// Flush all regs to struct Hart and update hartPCReg, changes reg map.
	void emitFlushRegsToHartAndMark(PhysAddr curPC);

	// Emit jmp away to a new PC, leaving this translation.
	// Tries to loop back to the beginning of this translation if possible.
	void emitPCRelativeJump(PhysAddr pcPhys, int32_t imm);

	// Helpers for FP state management
	void emitFaultOnFSOff(PhysAddr curPC);
	void emitMarkFSDirty();

	// State during generation of translations.
	PhysAddr thisTranslationStartPC;
	uint8_t *thisTranslationStartCode;
	PhysAddr thisTranslationCurrentPC;
	// emitFaultOnFSOff and emitMarkFSDirty are only needed once per translation
	bool thisTranslationFSKnownOn, thisTranslationFSKnownDirty;
	// Whether xmmNANBoxReg has been set.
	bool thisTranslationXMMNanMaskSet;

	// Some instruction need the correct value of hart->pc.
	// This stores the value hart->pc currently has, so that the needed diff can be applied.
	PhysAddr lastHartPC;
	// Updates hart->pc to point to the currently active translation.
	void emitUpdateHartPC(PhysAddr curPC);

	// If true, the last translation did an unconditional flush and return.
	bool jumpsAway;

	// How hart registers are mapped to x86 registers at this point
	// in a translation.
	// There are four states:
	// Not mapped
	// Mapped, clean (can be used)
	// Mapped, dirty (was written to), only lower 32bits
	// Mapped, dirty (was written to), all 64 bits
	const X86Reg NotMapped = X86Reg::RAX;
	const XMMReg NotMappedXMM = XMMReg::XMM0;
	template <typename T> struct RegMap {
		// gpr or xmm number. 0 (RAX and XMM0 are never mapped) means not mapped.
		T x86reg;
		// The latest address that used this mapping.
		// Must not be freed if it equals the currently translated one.
		PhysAddr usedAtPC;
		// If it has been written to and needs to be flushed.
		bool dirty;
		// Set if the lower 32 bits have been written to but not sign extended.
		bool bits32;
	};

	RegMap<X86Reg> rvRegsToX86[32];
	RegMap<XMMReg> rvFRegsToXMM[32];

	bool translateRVCInstruction(PhysAddr addr, uint16_t inst);
	bool translateInstruction(PhysAddr addr, uint32_t inst);

	bool translate(PhysAddr entry);

	template <typename Key, typename Result, size_t numBuckets, size_t entriesPerBucket>
	class CodeHashMap {
	public:
		CodeHashMap() { clear(); }
		void insert(Key key, Result result);
		bool lookup(Key key, Result *result);
		void clear();
	private:
		size_t bucketForKey(Key key);
		struct Bucket {
			struct Entry {
				Key key;
				Result result;
			} entries[entriesPerBucket];
			size_t numEntries;
		} buckets[numBuckets];
	};

	// Written during JIT code execution
	uint64_t jitScause;

	CodeHashMap<PhysAddr, uint8_t*, 1<<16, 2> codeHashMap;
	CodeHashMap<uint64_t, uint8_t*, 1<<5, 4> codeHashMapVirt;
};
