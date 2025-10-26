cpp_args = ['-static', '-fno-exceptions', '-fno-rtti', '-ffreestanding', '-nostdlib']

# TODO: Is there a better way?
link_script = files('kernel.ld')[0]
link_args = cpp_args + ['-Wl,-no-relax', '-Wl,--script,' +  link_script.full_path()]

# Extract newlib, then build and install it like this:
# export CC_FOR_TARGET=gcc AR_FOR_TARGET=ar READELF_FOR_TARGET=readelf RANLIB_FOR_TARGET=ranlib CFLAGS_FOR_TARGET="-g -O2 -fPIE -Wa,--noexecstack"
# . /configure --target=x86_64-pc-none --disable-multilib --disable-newlib-wide-orient --disable-newlib-atexit-dynamic-alloc --disable-newlib-supplied-syscalls
# make -j8
# make DESTDIR=<path-to-risc86>/prefix install -j8
custom_prefix = meson.current_source_dir() / '../prefix/usr/local/x86_64-pc-none/'

newlib = declare_dependency(compile_args: ['-isystem' + custom_prefix / 'include'],
                            link_args: ['-L' + custom_prefix / 'lib', '-lc'])

kernel = executable('kernel',
                    sources: ['main.cpp', 'mem.cpp', 'newlib_stubs.cpp', 'percpu.cpp', 'utils.cpp'],
					cpp_args: cpp_args, pie: true,
					link_args: link_args,
					link_depends: [link_script],
					dependencies: [newlib])
