project(
    'risc86',
    ['c', 'cpp'],
    version: '0',
    license: 'GPL-3.0-only',
    license_files: 'LICENSE.GPLv3',
    meson_version: '>= 1.4.0',
    default_options: ['c_std=gnu23'],
)

if meson.get_compiler('c').get_id() != 'clang'
    error('Add `--native-file clang.txt` when setting up')
endif

subdir('kernel')
subdir('efiloader')
