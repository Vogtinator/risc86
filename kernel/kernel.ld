OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
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
		*(.bss*)
	}
}
