.ONESHELL:

SUBDIRS = efiloader kernel

all: $(patsubst %, build-%, $(SUBDIRS))
clean: $(patsubst %, clean-%, $(SUBDIRS))

build-efiloader: build-kernel

build-%: %
	$(MAKE) -C $<

clean-%: %
	$(MAKE) -C $< clean

esp/EFI/BOOT/BOOTX64.EFI: build-efiloader
	install -m0644 -D efiloader/risc86.efi $@

test: esp/EFI/BOOT/BOOTX64.EFI
	qemu-system-x86_64 -accel kvm \
		-chardev stdio,mux=on,id=out -serial chardev:out -device isa-debugcon,chardev=out -monitor none \
		-nographic -no-reboot \
		-bios /usr/share/qemu/ovmf-x86_64.bin \
		-drive file=fat:esp,readonly=on,if=virtio

.PHONY: all clean
