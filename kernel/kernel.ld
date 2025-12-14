OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(kernel_entry)
SECTIONS
{
	. = 0xFFFF810000000000;
 	.text :
	{
		*(.text.entry)
		*(.text)
		*(.text.*)
	}

	. = ALIGN(4096);
	.data :
	{
		*(.data*)
		*(.rodata)
		*(.rodata*)
		. = ALIGN(8);
		_init_array_start = .;
		*(.init_array)
		_init_array_end = .;
		*(.bss*)
	}
}
