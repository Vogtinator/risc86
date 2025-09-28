# -mgeneral-regs-only: Until FPU initialization implemented
cpp_args_arch = ['-mgeneral-regs-only']
cpp_args = ['-fno-exceptions', '-fno-rtti', '-ffreestanding', '-nostdlib']
# TODO: Is there a better way?
link_script = files('kernel.ld')[0]
link_args = cpp_args + ['-Wl,--script', link_script.full_path()]

kernel = executable('kernel',
                    sources: 'main.cpp',
					cpp_args: cpp_args, pie: true,
					link_args: link_args,
					link_depends: [link_script])
