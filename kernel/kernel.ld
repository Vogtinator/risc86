OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(kernel_entry)
SECTIONS
{
	. = 0xFFFF800000000000;
 	.text :
	{
		kernel_entry = .;
		*(.text)
		*(.text.*)
	}

	. = ALIGN(4096);
	.data :
	{
		*(.data*)
		*(.rodata)
		*(.rodata*)
		*(.bss*)
	}
}
