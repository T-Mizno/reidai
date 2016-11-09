#ifndef __LINEAR_H__
#define __LINEAR_H__

#define MATRIX_IS_SINGULAR -1
#define GAUSS_SUCCESS 1
int Gauss(double **A, double *x, double *b, int N);
double **inversMatrix(double **Aori, int N);

#endif //___LINEAR_H__
