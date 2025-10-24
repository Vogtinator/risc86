extern "C" {
	#include <errno.h>
	#include <stdint.h>
	#include <stdio.h>
	#include <unistd.h>
	#include <sys/stat.h>
	#include <string.h>
	#include <signal.h>
}

static void dbgchar(char c)
{
	asm volatile("out %[c], %[port]" :: [port] "d" (uint16_t(0xe9)), [c] "a" (uint8_t(c)));
}

extern "C" int fstat(int fd, struct stat *st)
{
	if (fd < 3) {
		memset(st, 0, sizeof(*st));
		st->st_mode = S_IFCHR | 0066;
		return 0;
	}

	errno = ENOSYS;
	return -1;
}

extern "C" int getpid()
{
	errno = ENOSYS;
	return -1;
}

extern "C" int isatty(int fd)
{
	if (fd < 3) {
		return 1;
	}

	errno = EBADF;
	return -1;
}

extern "C" off_t lseek(int fd, off_t offset, int whence)
{
	errno = ENOSYS;
	return -1;
}

extern "C" int kill(int pid, int signo)
{
	errno = ENOSYS;
	return -1;
}

extern "C" int read(int fd, void *buf, size_t count)
{
	if (fd == 0) {
		return 0;
	}

	errno = EBADF;
	return -1;
}

extern "C" int write(int fd, const void *buf, size_t count)
{
	if (fd == 1 || fd == 2) {
		for(size_t i = 0; i < count; ++i)
			dbgchar(static_cast<const char*>(buf)[i]);

		return count;
	}

	errno = EBADF;
	return -1;
}

extern "C" void* sbrk(intptr_t incr)
{
	errno = -ENOMEM;
	return reinterpret_cast<void*>(-1);
}

extern "C" int close(int fd)
{
	errno = ENOSYS;
	return -1;
}

extern "C" void _exit(int rc)
{
	__builtin_trap();
}
