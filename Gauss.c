#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <time.h>

#include "myz.h"

typedef struct _GaussResult GaussResult;
struct _GaussResult
{
    int *flgBaseJs;
    int *baseJs;
    double **x;
    double **xs;
    int numOfXs;
};

double **createCloneMatrixD(double **A, int M, int N)
{
    double **clone;
    clone = newMatrixD(M, N);
    setMatrixD(A, clone, M, N);
    return clone;
}

double epsillon = 10e-10;

int almostEqual(double val1, double val2)
{
    return (fabs(val1 - val2) < epsillon);
}

int almostZero(double val)
{
    return almostEqual(val, 0.0);
}

void stdoutMatrixD(double **A, int M, int N)
{
    int i, j;

    for (i = 0; i < M; i++)
    {
        for (j = 0; j < N; j++)
        {
            printf(" %5.3f", A[i][j]);
        }
        printf("\n");
    }
    return;
}

void stdoutPMatrix(int *P, double **A, int M, int N)
{
    int i, j;
    for (i = 0; i < M; i++)
    {
        printf("P[%2d]=%2d: ", i, P[i]);
        for (j = 0; j < N; j++)
        {
            printf(" %5.3f", A[P[i]][j]);
        }
        printf("\n");
    }
    return;
}

GaussResult *newGaussResult(int M, int N)
{
    GaussResult *result = NULL;
    int j;

    result = (GaussResult *)malloc(sizeof(GaussResult));
    if (result == NULL)
    {
        exit(-1);
    }
    result->flgBaseJs = newVectorI(N);
    for (j = 0; j < N; j++)
    {
        result->flgBaseJs[j] = (1 != 1);
    }

    result->baseJs = NULL;

    result->x = newMatrixD(1, N);
    result->xs = NULL;
    result->numOfXs = 0;
    return result;
}

void freeGaussResult(GaussResult *result)
{
    if (result->flgBaseJs != NULL)
        free(result->flgBaseJs);
    if (result->baseJs != NULL)
        free(result->baseJs);
    if (result->x != NULL)
        freeMatrixD(result->x);
    if (result->xs != NULL)
        freeMatrixD(result->xs);
    free(result);
    return;
}

void stdoutGaussResult(GaussResult *result, int M, int N)
{
    int j;
    printf("flgBaseJs: ");
    for (j = 0; j < N; j++)
    {
        printf(" [%d", j);
        if (result->flgBaseJs[j])
        {
            printf(":b");
        }
        else
        {
            printf(":f");
        }
        printf("]");
    }
    printf("\n");

    printf("baseJs: ");
    for (j = 0; j < N - result->numOfXs; j++)
    {
        printf(" %d", result->baseJs[j]);
    }
    printf("\n");

    printf("x:\n");
    // stdoutMatrixD(result->x, 1, N);
    printf("numOfBaseJs: %d\n", (N - result->numOfXs));
    if (result->numOfXs > 0)
    {
        printf("xs:\n");
        //  stdoutMatrixD(result->xs, result->numOfXs, N);
    }
}

int searchMaxIAndSwapI(int *P, double **A, int pivotI, int pivotJ, int M, int N)
{
    int i, maxI, tmpI;
    double maxAbsValue;

    maxI = pivotI;
    maxAbsValue = fabs(A[P[pivotI]][pivotJ]);

    for (i = pivotI + 1; i < M; i++)
    {
        if (fabs(A[P[i]][pivotJ]) > maxAbsValue)
        {
            maxI = i;
            maxAbsValue = fabs(A[P[i]][pivotJ]);
        }
    }
    tmpI = P[pivotI];
    P[pivotI] = P[maxI];
    P[maxI] = tmpI;

    return 1;
}

int backward(int *P, double **A, double **b, double *x, GaussResult *r, int M, int N)
{
    int i, j;
    for (i = (N - r->numOfXs) - 1; i >= 0; i--)
    {
        double tmpb = 0.0;
        for (j = r->baseJs[i] + 1; j < N; j++)
        {
            tmpb += A[P[i]][j] * x[j];
        }
        x[r->baseJs[i]] = (b[P[i]][0] - tmpb) / A[P[i]][r->baseJs[i]];
    }

    return 0;
}

int gauss(int *P, double **A, double **b, GaussResult *r, int M, int N)
{
    int pivotI, pivotJ;
    int i, j;
    double pivot;

    pivotI = 0;
    pivotJ = 0;

    while (1 == 1)
    {
        if (pivotI > M - 1)
            break;
        while (1 == 1)
        {
            if (pivotJ > N - 1)
                break;
            searchMaxIAndSwapI(P, A, pivotI, pivotJ, M, N);
            if (!almostZero(A[P[pivotI]][pivotJ]))
                break;
            pivotJ++;
        }

        if (pivotJ > N - 1)
            break;

        r->flgBaseJs[pivotJ] = (1 == 1);

        pivot = A[P[pivotI]][pivotJ];
        for (i = pivotI + 1; i < M; i++)
        {
            A[P[i]][pivotJ] = A[P[i]][pivotJ] / pivot;
            for (j = pivotJ + 1; j < N; j++)
            {
                A[P[i]][j] -= A[P[i]][pivotJ] * A[P[pivotI]][j];
            }
            b[P[i]][0] -= A[P[i]][pivotJ] * b[P[pivotI]][0];
        }

        // printf("pivotI=%d, pivotJ=%d\n", pivotI, pivotJ);
        // stdoutPMatrix(P, A, M, N);
        // stdoutPMatrix(P, b, M, 1);

        pivotI++;
        pivotJ++;
    }

    // numOfXs
    r->numOfXs = N;
    for (j = 0; j < N; j++)
    {
        if (r->flgBaseJs[j])
            r->numOfXs--;
    }

    // baseJs
    r->baseJs = newVectorI(N - r->numOfXs);
    {
        int baseCount = 0;
        for (j = 0; j < N; j++)
        {
            if (r->flgBaseJs[j])
            {
                r->baseJs[baseCount] = j;
                baseCount++;
            }
        }
    }

    r->xs = newMatrixD(r->numOfXs, N);
    for (i = 0; i < r->numOfXs; i++)
    {
        for (j = 0; j < N; j++)
        {
            r->xs[i][j] = 0.0;
        }
    }
    {
        int freeVarCount = 0;
        for (j = 0; j < N; j++)
        {
            if (!r->flgBaseJs[j])
            {
                r->xs[freeVarCount][j] = 1.0;
                freeVarCount++;
            }
        }
    }

    backward(P, A, b, r->x[0], r, M, N);

    // xs
    {
        double **zerob;
        zerob = newMatrixD(M, 1);
        for (i = 0; i < M; i++)
        {
            zerob[i][0] = 0.0;
        }
        for (i = 0; i < r->numOfXs; i++)
        {
            backward(P, A, zerob, r->xs[i], r, M, N);
        }
        freeMatrixD(zerob);
    }
    return 0;
}

double errorX(double **A, double *x, double **b, int M, int N)
{
    int i, j;
    double tmp;

    double error = 0.0;

    for (i = 0; i < M; i++)
    {
        tmp = 0.0;
        for (j = 0; j < N; j++)
        {
            tmp += A[i][j] * x[j];
        }
        error += fabs(b[i][0] - tmp);
    }
    return error;
}

double errorXs(double **A, GaussResult *r, int M, int N)
{
    double **b = newMatrixD(M, 1);
    double error = 0.0;
    int i;

    for (i = 0; i < M; i++)
    {
        b[i][0] = 0.0;
    }

    for (i = 0; i < r->numOfXs; i++)
    {
        error += errorX(A, r->xs[i], b, M, N);
    }
    return error;
}

int testFrame(double **aA, double **ab, int M, int N)
{
    GaussResult *r = newGaussResult(M, N);
    double **A = createCloneMatrixD(aA, M, N);
    double **b = createCloneMatrixD(ab, M, 1);
    int *P = newVectorI(M);
    int i;

    for (i = 0; i < M; i++)
    {
        P[i] = i;
    }

    // printf("M=%d, N=%d\n", M, N);
    // printf("A\n");
    // stdoutMatrixD(A, M, N);
    // printf("b\n");
    // stdoutMatrixD(b, M, 1);

    {
        time_t start_t, end_t;
        start_t = clock();
        gauss(P, A, b, r, M, N);
        end_t = clock();
        // printf("Clock Time: %ld\n", end_t - start_t);
        printf("%d, %ld\n", M, end_t - start_t);
    }

    // stdoutGaussResult(r, M, N);

    // printf("errorX  %f\n", errorX(aA, r->x[0], ab, M, N));
    // printf("errorXs %f\n", errorXs(aA, r, M, N));

    freeMatrixD(A);
    freeMatrixD(b);
    free(P);
    freeGaussResult(r);

    return 1;
}

int main(void)
{
    double **testA, **testb;
    int M, N;

    // N=4; M = readMatrixFromFileD("sampleA58.dat", &testA, N); M = readMatrixFromFileD("sampleb58.dat", &testb, 1);
    // N=3; M = readMatrixFromFileD("sampleA20.dat", &testA, N); M = readMatrixFromFileD("sampleb20.dat", &testb, 1);
    // N=1; M = readMatrixFromFileD("sample1a.dat", &testA, N); M = readMatrixFromFileD("sample1b.dat", &testb, 1);
    // N=5; M = readMatrixFromFileD("sample2a.dat", &testA, N); M = readMatrixFromFileD("sample2b.dat", &testb, 1);

    for (M = 2000; M <= 2005; M++)
    {
        {
            int i, j;
            N = M;
            // M = 50;
            // N = 50;
            testA = newMatrixD(M, N);
            testb = newMatrixD(M, 1);
            for (i = 0; i < M; i++)
            {
                double tmp = 0.0;
                for (j = 0; j < N; j++)
                {
                    testA[i][j] = sin(0.01 * (double)(i * j)) + cos((double)(i + j));
                    tmp += testA[i][j] * (double)j;
                }
                testb[i][0] = tmp;
            }
        }
        testFrame(testA, testb, M, N);

        freeMatrixD(testA);
        freeMatrixD(testb);
    }

    return 0;
}
