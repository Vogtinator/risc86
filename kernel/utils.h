#pragma once

#include <math.h>

template<typename T> T min(T a, T b) {
    return (a < b) ? a : b;
}

template<typename T> T max(T a, T b) {
    return (a > b) ? a : b;
}

template <typename T> T genericFMA(T a, T b, T c) {
    return a * b + c;
}

/* Newlib's libm does not actually fuse it, so why bother?

template <> inline float genericFMA(float a, float b, float c) {

    return fmaf(a, b, c);
}
template <> inline double genericFMA(double a, double b, double c) {
    return fma(a, b, c);
}
*/

template <typename T> T genericSqrt(T a);
template <> inline float genericSqrt(float a) {
    return sqrtf(a);
}
template <> inline double genericSqrt(double a) {
    return sqrt(a);
}

extern "C" void panic(const char *fmt, ...) __attribute__((format(printf, 1, 2))) __attribute__((noreturn));
