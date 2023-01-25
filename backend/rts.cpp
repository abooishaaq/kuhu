#include <arrayfire.h>
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <vector>

af::array zerosMatrixF(int rows, int cols) {
    return af::constant(0, rows, cols, f64);
}

af::array zerosMatrixI(int rows, int cols) {
    return af::constant(0, rows, cols, s64);
}

af::array constantMatrixF(int rows, int cols, double val) {
    return af::constant(val, rows, cols, f64);
}

af::array constantMatrixI(int rows, int cols, int val) {
    return af::constant(val, rows, cols, s64);
}

af::array randnMatrixF(int rows, int cols) {
    return af::randn(rows, cols, f64);
}

af::array randnMatrixI(int rows, int cols) {
    return af::randn(rows, cols, s64);
}

af::array randuMatrixF(int rows, int cols) {
    return af::randu(rows, cols, f64);
}

af::array randuMatrixI(int rows, int cols) {
    return af::randu(rows, cols, s64);
}

af::array multiply(af::array a, af::array b) { return af::matmul(a, b); }

af::array add(af::array a, af::array b) {
    return a + b;
}

void printArray(af::array arr) { af_print(arr); }
