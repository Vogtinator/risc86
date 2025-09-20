#include <cstdint>

__attribute__((noinline)) void putc(char c)
{
	asm volatile("out %[c], %[port]" :: [port] "d" (uint16_t(0xe9)), [c] "a" (uint8_t(c)));
}

__attribute__((noinline)) void puts(const char *s)
{
	while(*s)
		putc(*s++);
}

__attribute__((noreturn)) __attribute__((section(".text.entry"))) void kernel_entry()
{
	puts("Starting kernel...\n");
	asm volatile("push %rax; pop %rax");
	puts("Hello World!\n");

	for(;;)
		asm volatile("cli; hlt");
}
