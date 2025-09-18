#include <cstdint>

extern "C" __attribute__((noreturn)) void kernel_entry()
{
	static const char hello[] = "Hello World!\n";
	for(int i = 0; i < sizeof(hello) - 1; ++i)
		asm volatile("out %[c], %[port]" :: [port] "d" (uint16_t(0xe9)), [c] "a" (uint8_t(hello[i])));

	for(;;)
		asm volatile("cli; hlt");
}
