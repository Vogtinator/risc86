efi_c_flags_arch = ['-target', 'x86_64-unknown-windows', '-mno-red-zone', '-fshort-wchar']
efi_c_flags = efi_c_flags_arch + ['-Wall', '-Wextra', '-ffreestanding']
efi_link_flags = [
    '-target', 'x86_64-unknown-windows',
    '-nostdlib',
    '-Wl,-entry:efi_main',
    '-Wl,-subsystem:efi_application',
    '-fuse-ld=lld-link',
]

# Find gnu-efi, but only use $includedir/efi to avoid making other includes like stdint.h visible.
gnu_efi = dependency('gnu-efi')

# Convert the kernel ELF into kernel.bin for embedding
objcopy = find_program('objcopy')
kernel_bin = custom_target(
    input: kernel,
    output: 'kernel.bin',
    command: [objcopy, '-O', 'binary', '@INPUT@', '@OUTPUT@'],
)
efi_c_flags += ['-DKERNEL_BIN_PATH="' + kernel_bin.full_path() + '"']

risc86_efi = executable(
    'risc86.efi',
    sources: ['main.c', kernel_bin],
    include_directories: gnu_efi.get_variable('includedir') + '/efi',
    c_args: efi_c_flags,
    link_args: efi_link_flags,
    build_by_default: true,
)

# Prepare a directory with ESP layout:
# $builddir/esp/EFI/BOOT/BOOTX64.EFI -> ../../../risc86.efi
qemu_esp_dir = meson.current_build_dir() / 'esp'
qemu_esp_fill = custom_target(
    input: risc86_efi,
    output: 'esp_filled',
    command: [
        'sh',
        '-ec', '''
mkdir -p "$0"/EFI/BOOT/
ln -frs "$2" "$0"/EFI/BOOT/BOOTX64.EFI
touch "$1"
''',
        qemu_esp_dir,
        '@OUTPUT@',
        '@INPUT@',
    ],
)

# Run QEMU with fat emulation for the ESP
qemu = find_program('qemu-system-x86_64')
qemu_args = [
    '-accel', 'kvm',
    '-m', '1G',
    '-chardev', 'stdio,mux=on,id=out',
    '-serial', 'chardev:out',
    '-device', 'isa-debugcon,chardev=out',
    '-monitor', 'none',
    '-nographic',
    '-no-reboot',
    '-bios', '/usr/share/qemu/ovmf-x86_64.bin',
    '-drive', 'file=fat:' + qemu_esp_dir + ',readonly=on,if=virtio',
]

custom_target(
    'qemu',
    output: 'none',
    command: [qemu, qemu_args],
    console: true,
    depends: [qemu_esp_fill],
    build_always_stale: true,
)
