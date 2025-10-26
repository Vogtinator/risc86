#pragma once

extern "C" void panic(const char *fmt, ...) __attribute__((format(printf, 1, 2))) __attribute__((noreturn));
