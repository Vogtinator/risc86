#pragma once

template<typename T> T max(T a, T b) {
    return (a > b) ? a : b;
}

extern "C" void panic(const char *fmt, ...) __attribute__((format(printf, 1, 2))) __attribute__((noreturn));
