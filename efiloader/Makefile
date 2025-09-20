CFLAGS ?= -target x86_64-unknown-windows -std=c23 -Wall -Wextra -ffreestanding -fshort-wchar -mno-red-zone -I /usr/include/efi
LDFLAGS ?= -target x86_64-unknown-windows -nostdlib -Wl,-entry:efi_main -Wl,-subsystem:efi_application -fuse-ld=lld

all: risc86.efi

%.o: %.c
	clang $(CFLAGS) -c $^ -o $@

risc86.efi: main.o
	clang $(LDFLAGS) $^ -o $@

clean:
	rm -f *.o risc86.efi

.PHONY: clean
