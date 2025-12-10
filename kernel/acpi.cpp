#include <stdio.h>

#include "acpi.h"
#include "mem.h"
#include "uacpi/uacpi.h"
#include "utils.h"

uacpi_status uacpi_kernel_get_rsdp(uacpi_phys_addr *rsdp_out)
{
	*rsdp_out = kernel_params.xsdp_phys;
	return UACPI_STATUS_OK;
}

void *uacpi_kernel_map(uacpi_phys_addr addr, uacpi_size len)
{
	(void) len;
	return phys_to_virt<void>(addr);
}

void uacpi_kernel_unmap(void *addr, uacpi_size len)
{
	(void) addr; (void) len;
}

void uacpi_kernel_log(uacpi_log_level l, const uacpi_char *s)
{
	(void) l;
	printf("%s", s);
}

static uint8_t uacpiTempBuff[4096];

void setupACPI()
{
	auto ret = uacpi_setup_early_table_access(uacpiTempBuff, sizeof(uacpiTempBuff));
	if (ret != UACPI_STATUS_OK)
		panic("uACPI failed with %d", ret);
}
