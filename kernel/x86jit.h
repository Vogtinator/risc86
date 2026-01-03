#pragma once

#include "hart.h"
#include "mem.h"

class X86JIT
{
public:
	X86JIT();
	// Allocate memory for the generated code
	void init();

	// Try to generate (if necessary) and run code for the current hart state.
	// Returns false if fallback to the interpreter is neccessary.
	bool tryJit(Hart *hart, PhysAddr pcPhys);

	// Discard all translations.
	void reset();
private:
	const size_t JIT_REGION_SIZE = 8*1024*1024; // 8 MiB
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
	inline bool regREXBit(X86Reg r) { return static_cast<uint8_t>(r) & 0b1000; }
	inline uint8_t regLow3Bits(X86Reg r) { return static_cast<uint8_t>(r) & 0b0111; }

	using RVReg = uint8_t;

	void emitREX(bool w, bool r, bool x, bool b);
	void emitMovImmediate32(X86Reg x86Reg, int32_t imm);
	void emitAddImmediate(X86Reg x86Reg, int32_t imm);
	void emitMovRegReg(X86Reg from, X86Reg to);
	void emitXorRegReg(X86Reg x86Reg);
	void emitCliHlt(); // For debugging

	// Low-level helpers for RV register management
	const X86Reg hartPtrReg = X86Reg::RDI;
	void emitLoadRVReg(RVReg rvReg, X86Reg x86Reg);
	void emitLoadPC(X86Reg x86Reg);
	void emitStorePC(X86Reg x86Reg);
	void emitAddPC(int32_t value);
	void emitStoreRVReg64(X86Reg x86Reg, RVReg rvReg);
	void emitSExtX86Reg(X86Reg x86Reg); // 32->64 sign extension
	void emitRet(uint32_t retVal);

	// High-level helpers for RV register management
	// Use %r8-%r15, but skip %r12 as it has a different meaning in ModRM...
	const X86Reg x86DynRegFirst = X86Reg::R8, x86DynRegLast = X86Reg::R15;
	// Flushes RV reg to struct Hart, does not change reg map.
	void emitFlushRVReg(RVReg rvReg);
	// Changes reg map accordingly.
	void markRVRegFlushed(RVReg rvReg);
	X86Reg findFreeDynReg();
	X86Reg mapRVRegForRead(RVReg rvReg, bool bits32Ok);
	X86Reg mapRVRegForRead64(RVReg rvReg);
	X86Reg mapRVRegForRead32(RVReg rvReg);
	X86Reg mapRVRegForWrite32(RVReg rvReg);
	X86Reg mapRVRegForWrite64(RVReg rvReg);
	X86Reg mapRVRegForReadWrite64(RVReg rvReg);
	void emitFlushRegsToHart();

	// Emit jmp away to a new PC, leaving this translation.
	// Tries to loop back to the beginning of this translation if possible.
	void emitPCRelativeJump(PhysAddr pcPhys, int32_t imm);

	// For load/store: If carry set, leave the translation with given scause.
	void emitLeaveOnMemFault(PhysAddr curPC, uint32_t scause);

	// State during generation of translations.
	PhysAddr thisTranslationStartPC;
	uint8_t *thisTranslationStartCode;
	PhysAddr thisTranslationCurrentPC;

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
	struct {
		// gpr number. 0 (RAX is ever mapped) means not mapped.
		X86Reg x86reg;
		// The latest address that used this mapping.
		// Must not be freed if it equals the currently translated one.
		PhysAddr usedAtPC;
		// If it has been written to and needs to be flushed.
		bool dirty;
		// Set if the lower 32 bits have been written to but not sign extended.
		bool bits32;
	} rvRegsToX86[32];

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

	CodeHashMap<PhysAddr, uint8_t*, 64, 64> codeHashMap;
};
