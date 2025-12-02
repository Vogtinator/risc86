RISC86 - RISC-V System Emulator in x86 Kernel Space
===

RISC86 is an EFI executable that provides an environment for running RISC-V operating systems on x86 hardware.

Why
---

There are multiple ways to run RISC-V software on other architectures like x86. The most common options are to use QEMU in either userspace emulation mode (only specific processes are emulated, syscalls are passed through to the host kernel) or system emulation mode (full kernel- and userspace are emulated, with virtual devices).

Full system emulation is rather slow, as it has to spend time emulating devices and the MMU for all memory access.

Userspace emulation usually has less overhead, but there are some downsides: Sometimes kernel emulation is necessary or QEMU is lacking some syscalls needed by programs (like some tty ioctls currently, which leads to a missing shell prompt with recent glibc).

The idea of this project is to write a full system emulator with minimal overhead, by passing through physical (resp. QEMU virtual) hardware as much as possible. Running in kernel mode makes it possible to use the x86 MMU to perform translations set up by the emulated RISC-V system. Some types of devices (PCI, other kinds of MMIO, ...) may be passed through directly, others have to be emulated to some extent (interrupt cotrollers, timers, serial, ...).

Building
---
Make sure the git submodules are up-to-date:

```
git submodule init
git submodule update
```

First, newlib needs to be built with the flags mentioned in kernel/meson.build:

```
wget ftp://sourceware.org/pub/newlib/newlib-4.5.0.20241231.tar.gz
tar xf newlib-4.5.0.20241231.tar.gz
cd newlib-4.5.0.20241231
./configure --target=x86_64-pc-none --disable-multilib --disable-newlib-wide-orient --disable-newlib-atexit-dynamic-alloc --disable-newlib-supplied-syscalls
make -j8
make DESTDIR="$PWD/../prefix" install -j8
```

Then meson can be used as usual for building:

```
meson compile -C build
```

Running
---

To start RISC86 in QEMU, use the `qemu` target:

```
meson compile -C build qemu
```

You might have to adjust the path to OVMF in efiloader/meson.build.

To provide a kernel/initrd pair for execution, put them into the `efiloader/esp/` directory of the build dir:

```
cp <linux dir>/arch/riscv/boot/Image build/efiloader/esp/Image
cp <some initrd> build/efiloader/esp/initrd
```

Design
---

The emulator shares physical and virtual memory with the emulated operating system, so the memory layout had to be carefully designed: Only physical memory ranges not used by the emulator are passed to the emulated system.

For virtual memory it's a bit more complex, as there's no control over the virtual address ranges used by the operating system. RISC86 makes use of the Sv39 virtual addressing scheme presented to the guest while using traditional 48-bit paging on the x86 side, so the full virtual memory can be used by both:

The RISC-V side only has 39 virtual address bits, values outside of these ranges are "non-canonical":

```
0x0000000000000000 - 0x0000003FFFFFFFFF: RISC-V valid
0xFFFFFFC000000000 - 0xFFFFFFFFFFFFFFFF: RISC-V valid
```

The native x86 code has 48 virtual address bits, allowing for wider ranges:

```
0x0000000000000000 - 0x00007FFFFFFFFFFF: x86 valid (unused ATM)
0xFFFF800000000000 - 0xFFFFFFFFFFFFFFFF: x86 valid
```

The emulator uses the 0xFFFF800000000000 - 0xFFFFFFBFFFFFFFFF range for virtual addresses, which is valid for x86 but non-canonical for RISC-V.

Other parts TBD.

License
---

GPL-3.0-only, see LICENSE.GPLv3.