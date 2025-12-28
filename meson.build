project(
    'risc86',
    ['c', 'cpp'],
    version: '0',
    license: 'GPL-3.0-only',
    license_files: 'LICENSE.GPLv3',
    meson_version: '>= 1.4.0',
    # optimize=0 breaks code with clang
    default_options: ['c_std=gnu23', 'cpp_std=gnu++23', 'optimize=3', 'b_lto=true'],
)

if meson.get_compiler('c').get_id() != 'clang'
    error('Add `--native-file clang.txt` when setting up')
endif

uacpi = subproject('uACPI')
subdir('kernel')
subdir('efiloader')
