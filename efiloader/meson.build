efi_c_flags_arch = ['-target', 'x86_64-unknown-windows', '-mno-red-zone', '-fshort-wchar', '-mgeneral-regs-only']
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

# Convert the logo PNG to BGRA
magick = find_program('magick')
logo_bin = custom_target(
    input: files('../logo/logo.png'),
    output: 'logo.bgra',
    command: [magick, '@INPUT@', '-scale', '512x512', '@OUTPUT@'],
)
efi_c_flags += ['-DLOGO_SIZE=512']

# Workaround ninja behaviour: The path in the depfile must be identical
# to the ninja target name, i.e. efiloader/kernel.bin, so need to use
# a relative path here as well.
efi_c_flags += ['--embed-dir=efiloader']

risc86_efi = executable(
    'risc86.efi',
    sources: ['main.c', kernel_bin, logo_bin],
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
    '-cpu', 'host',
    '-m', '4G',
    '-chardev', 'stdio,mux=on,id=out',
    '-serial', 'chardev:out',
    '-device', 'isa-debugcon,chardev=out',
    '-monitor', 'none',
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
