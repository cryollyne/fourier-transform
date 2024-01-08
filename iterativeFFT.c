#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <complex.h>

static void fftIteration(int iteration, complex double *data, int dataLength, complex double w) {
    for (int i = 0; i < dataLength; i++) {
        if (i & (1 << iteration)) {
            int p = i & ((1 << iteration) - 1);
            data[i] *= cpow(w, p + 1);
        }
    }

    complex double *buffer = malloc(sizeof(complex double[dataLength]));

    for (int i = 0; i < dataLength; i++) {
        _Bool subtract = i & (1 << iteration);

        complex double n1 = data[i ^ (1 << iteration)];
        complex double n2 = data[i];

        if (subtract)
            buffer[i] = n1 - n2;
        else
            buffer[i] = n1 + n2;
    }

    memcpy(data, buffer, sizeof(complex double[dataLength]));
    free(buffer);
}

static int bitReverse(int data, int length) {
    int newData = 0;
    for (int i = 0; i < length; i++) {
        newData <<= 1;
        if (data & (1 << i))
            newData += 1;
    }
    return newData;
}

void fft(complex double *data, int log2ofLength, _Bool inverse) {
    int length = 1 << log2ofLength;
    complex double *buffer = malloc(length * sizeof(complex double));
    // bit reversal
    for (int i = 0; i < length; i++)
        buffer[bitReverse(i, log2ofLength)] = data[i];

    // iterations
    for (int i = 0; i < log2ofLength; i++) {
        double angle = (2*M_PI) / (float)(1 << (i + 1));
        if (inverse)
            angle = -angle;
        complex double w = cos(angle) + I * sin(angle);
        fftIteration(i, buffer, length, w);
    }

    // reverse order
    for(int i = 0; i < length/2; i++){
        complex double temp;
        temp = buffer[i];
        buffer[i] = buffer[length - i - 1];
        buffer[length - i - 1] = temp;
    }

    // divide by length if inverse
    if (inverse)
        for (int i = 0; i < length; i++)
            buffer[i] /= length;

    memcpy(data, buffer, length * sizeof(complex double));
    free(buffer);
}
