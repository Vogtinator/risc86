CXX := clang++
CXXFLAGS_ARCH ?= -mgeneral-regs-only -mno-redzone
CXXFLAGS ?= $(CXXLAGS_ARCH) -g -fno-exceptions -fno-rtti -ffreestanding -Os -nostdlib -fPIE
LDFLAGS ?= $(CXXFLAGS) -Wl,--script kernel.ld

all: kernel.bin

%.o: %.cpp | Makefile
	$(CXX) $(CXXFLAGS) -c $^ -o $@

kernel.elf: main.o | kernel.ld
	$(CXX) $(LDFLAGS) $^ -o $@

kernel.bin: kernel.elf
	objcopy -O binary $^ $@

clean:
	rm -f *.o kernel.elf kernel.bin
	
.PHONY: clean
