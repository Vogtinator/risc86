c_args = ['-ffreestanding', '-nostdlib']

c_args += ['-DUACPI_BAREBONES_MODE']

cpp_args = c_args + ['-fno-exceptions', '-fno-rtti']

# TODO: Is there a better way?
link_script = files('kernel.ld')[0]
link_args = cpp_args + ['-Wl,-no-relax', '-Wl,--script,' + link_script.full_path()]

# Extract newlib, then build and install it like this:
# export CC_FOR_TARGET=gcc AR_FOR_TARGET=ar READELF_FOR_TARGET=readelf RANLIB_FOR_TARGET=ranlib CFLAGS_FOR_TARGET="-g -O2 -fPIE -Wa,--noexecstack"
# ./configure --target=x86_64-pc-none --disable-multilib --disable-newlib-wide-orient --disable-newlib-atexit-dynamic-alloc --disable-newlib-supplied-syscalls
# make -j8
# make DESTDIR=<path-to-risc86>/prefix install -j8
custom_prefix = meson.current_source_dir() / '../prefix/usr/local/x86_64-pc-none/'

newlib = declare_dependency(
    compile_args: ['-isystem' + custom_prefix / 'include'],
    link_args: ['-L' + custom_prefix / 'lib', '-lc', '-lm'],
)

libfdt_path = '../dtc/libfdt'
libfdt_sources = [
    'fdt.c',
    'fdt_addresses.c',
    'fdt_check.c',
    'fdt_empty_tree.c',
    'fdt_overlay.c',
    'fdt_ro.c',
    'fdt_rw.c',
    'fdt_strerror.c',
    'fdt_sw.c',
    'fdt_wip.c',
]
libfdt_sources_abs = []
foreach filename : libfdt_sources
    libfdt_sources_abs += libfdt_path / filename
endforeach

libfdt_a = static_library(
    'fdt',
    sources: libfdt_sources_abs,
    include_directories: [libfdt_path],
    c_args: c_args,
    dependencies: [newlib],
)

libfdt = declare_dependency(include_directories: [libfdt_path], link_with: libfdt_a)

libuacpi_a = static_library(
    'uacpi',
    sources: uacpi.get_variable('sources'),
    include_directories: uacpi.get_variable('includes'),
    c_args: c_args,
)

libuacpi = declare_dependency(include_directories: [uacpi.get_variable('includes')], link_with: libuacpi_a)

kernel = executable(
    'kernel',
    sources: [
        'acpi.cpp',
        'devicetree.cpp',
        'hart.cpp',
        'hpet.cpp',
        'main.cpp',
        'mem.cpp',
        'newlib_stubs.cpp',
        'pci.cpp',
        'percpu.cpp',
        'rvmmu.cpp',
        'sbi.cpp',
        'smp.cpp',
        'utils.cpp',
        'x86interrupts.cpp',
        'x86jit.cpp',
        'x86mmu.cpp',
    ],
    cpp_args: cpp_args,
    pie: true,
    link_args: link_args,
    link_depends: [link_script],
    dependencies: [newlib, libfdt, libuacpi],
)
