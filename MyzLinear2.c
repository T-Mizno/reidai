#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <assert.h>

#include <time.h>


#define DTYPE double

#define EPSILLON 10e-15

struct _MyzMatrix {
    int _M;
    int _N;

    int isColMode;

    DTYPE **_entity;
};
typedef struct _MyzMatrix MyzMatrix;

struct _GaussSystem {
    MyzMatrix *A;
    int *p;
    MyzMatrix *b;
    MyzMatrix *x;
    //MyzMatrix *xs;
    int rank;
    int *baseVars;
    int isSolvable;
    DTYPE solveTime;
};
typedef struct _GaussSystem GaussSystem;

int matM(MyzMatrix *A);
int matN(MyzMatrix *A);

DTYPE at(MyzMatrix *A, int i, int j);
int set(MyzMatrix *A, int i, int j, DTYPE val);

MyzMatrix *matCopy(MyzMatrix *A);
DTYPE matDiff1(MyzMatrix *A, MyzMatrix *B);

/////////////////////////////////////////////////////////

int isZero(DTYPE v) { return fabs(v) < EPSILLON ; }

int *newVectorI(int M) {
    int *r;
    r = (int *) malloc(sizeof(int) * M);
    if(r == NULL) {
        printf("Cannot create an int vector.\n");
        exit(-1);
    }

    return r;
}

int matM(MyzMatrix *A) { return A->_M; }
int matN(MyzMatrix *A) { return A->_N; }

void matFill(MyzMatrix *A, DTYPE val) {
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(A, i, j, val);
        }
    }
    return;
}

MyzMatrix *newMyzMatrix(int M, int N) {
    MyzMatrix *Mat;
    int i;

    Mat = (MyzMatrix *) malloc(sizeof(MyzMatrix));
    if(Mat == NULL) {
        printf("Cannot create MyzMatrix.\n");
        exit(-1);
    }

    Mat->_entity = (DTYPE **) malloc(sizeof(DTYPE *) * M);
    if(Mat->_entity == NULL) {
        printf("Cannot create entity of MyzMatrix.\n");
        free(Mat);
        exit(-1);
    }

    for(i=0; i<M; i++) {
        Mat->_entity[i] = (DTYPE *) malloc(sizeof(DTYPE) * N);
        if(Mat->_entity[i] == NULL) {
            printf("Cannot create row %d.\n", i);
            for(;i>=0; i--) free(Mat->_entity[i]);
            free(Mat);
            exit(-1);
        }
    }

    Mat->_M = M;
    Mat->_N = N;

    Mat->isColMode = (1 != 1); // false

    matFill(Mat, 0.0);

    return Mat;
}

void setRandVal(MyzMatrix *a, int seed, DTYPE rMin, DTYPE rMax) {
    int i, j;
    srand(seed);
    for(i=0; i<matM(a); i++) {
        for(j=0; j<matN(a); j++) {
            set(a, i, j, (rMax-rMin)*((DTYPE)rand()/(DTYPE)RAND_MAX)+rMin);
        }
    }
}

MyzMatrix *newRandMyzMatrix(int seed, DTYPE rMin, DTYPE rMax, int m, int n) {
    MyzMatrix *mat = newMyzMatrix(m, n);

    setRandVal(mat, seed, rMin, rMax);

    return mat;
}

MyzMatrix *newMyzMatrixFromArray(DTYPE a[], int M, int N) {
    int i, j;
    MyzMatrix *C = newMyzMatrix(M, N);

    for(i=0; i<M; i++) {
        for(j=0; j<N; j++) {
            set(C, i, j, a[i*N + j]);
        }
    }

    return C;
}

MyzMatrix *newColAccessMyzMatrix(int M, int N) {
    MyzMatrix *mat;

    mat = newMyzMatrix(N, M);
    mat->isColMode = (1 == 1); // true
    mat->_M = M;
    mat->_N = N;

    return mat;
}

void freeMyzMatrix(MyzMatrix *A) {
    int i;
    if(A == NULL) return;

    if(A->isColMode) {
        A->_M = A->_N;
    }

    for(i=0; i<matM(A); i++) {
        free(A->_entity[i]);
    }
    free(A->_entity);
    free(A);
}

DTYPE at(MyzMatrix *A, int i, int j) {
    assert( (i >= 0) && (i < matM(A)) );
    assert( (j >= 0) && (j < matN(A)) );

    if(A->isColMode) {
        return A->_entity[j][i];
    }

    return A->_entity[i][j];
}

int set(MyzMatrix *A, int i, int j, DTYPE val) {
    assert( (i >= 0) && (i < matM(A)) );
    assert( (j >= 0) && (j < matN(A)) );

    if(A->isColMode) {
        A->_entity[j][i] = val;
        return 0;
    }

    A->_entity[i][j] = val;
    return 0;
}

void stdoutMyzMatrix(MyzMatrix *A) {
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            printf(" %11.9f", at(A, i,j));
        }
        printf("\n");
    }
}

void pStdout(int *p, MyzMatrix *A) {
    int i, j;
    for(i=0; i<matM(A); i++) {
        printf("%4d:", p[i]);
        for(j=0; j<matN(A); j++) {
            //printf(" %5.3f", at(A, i,j));
            printf(" %11.9f", at(A, p[i], j));
        }
        printf("\n");
    }
}

void freeGaussSystem(GaussSystem *gs) {
    if(gs->A != NULL) freeMyzMatrix(gs->A);
    if(gs->b != NULL) freeMyzMatrix(gs->b);
    if(gs->p != NULL) free(gs->p);
    if(gs->x != NULL) freeMyzMatrix(gs->x);
    if(gs->baseVars != NULL) free(gs->baseVars);
    free(gs);
}

int searchMaxIAndSwapI(int *P, MyzMatrix *A, int pivotI, int pivotJ)
{
    int i, maxI, tmpI;
    DTYPE maxAbsValue;
    int M = matM(A);

    maxI = pivotI;
    maxAbsValue = at(A, P[pivotI], pivotJ);

    for(i=pivotI+1; i<M; i++) {
        if(fabs(at(A, P[i], pivotJ))> maxAbsValue) {
            maxI = i;
            maxAbsValue = fabs(at(A, P[i], pivotJ));
        }
    }
    tmpI = P[pivotI];
    P[pivotI] = P[maxI];
    P[maxI] = tmpI;

    return 1;
}

GaussSystem *gauss2(MyzMatrix *aA, MyzMatrix *ab) {
    assert(matM(ab) >= matM(aA));

    MyzMatrix *A = matCopy(aA);
    MyzMatrix *b = matCopy(ab);
    int *p = newVectorI(matM(A));
    MyzMatrix *x = newMyzMatrix(matN(A), 1);
    int baseVarsNum = 0;
    int *baseVarsFlg = newVectorI(matN(A));
    int *baseVars = newVectorI(matN(A));
    int isSolvable = (1==1); // true
    clock_t start_t, end_t;
    DTYPE solveTime;
    int pivotI, pivotJ, i, j;

    GaussSystem *result = NULL;

    start_t = clock();

    for(i=0; i<matM(A); i++) { p[i] = i; }
    for(j=0; j<matN(A); j++) { baseVarsFlg[j] = (1 !=1);  }

    // forward
    pivotI = -1; pivotJ = -1;
    while(1==1) {
        pivotI++; pivotJ++;
        if(  (pivotI >= matM(A))  ||  (pivotJ >= matN(A))  ) {
            break;
        }

        searchMaxIAndSwapI(p, A, pivotI, pivotJ);
        if(isZero(at(A, p[pivotI], pivotJ))) {
            continue;
        }
        baseVarsFlg[pivotJ] = (1==1); //true
        for(i=pivotI+1; i<matM(A); i++) {
            DTYPE pVal = at(A, p[i], pivotJ)/at(A, p[pivotI], pivotJ);
            set(A, p[i], pivotJ,  pVal);
            for(j=pivotJ+1; j<matN(A); j++) {
                set(A, p[i], j, at(A, p[i], j) - pVal*at(A, p[pivotI], j));
            }
            set(b, p[i], 0, at(b, p[i], 0)-pVal*at(b, p[pivotI], 0));
        }
    }  // end of forward

    //backward
    matFill(x, 1.0);
    for(j=0, baseVarsNum=0; j<matN(A); j++) {
        if(baseVarsFlg[j]) {
            baseVars[baseVarsNum] = j;
            baseVarsNum++;
        }
    }
    for(i=baseVarsNum; i<matM(A); i++) {
        isSolvable = isSolvable && isZero(at(b, p[i], 0)/((DTYPE)matN(A)));
    }
    for(i=baseVarsNum-1; i>=0; i--) {
        DTYPE sum = 0.0;
        for(j=baseVars[i]+1; j<matN(A); j++) {
            sum += at(A, p[i], j) * at(x, j, 0);
        }
        set(x, baseVars[i], 0, (at(b, p[i], 0)-sum)/at(A, p[i], baseVars[i]));
    }

    end_t = clock();
    solveTime = (DTYPE)(end_t - start_t)/CLOCKS_PER_SEC;


    //build result
    result = (GaussSystem *) malloc(sizeof(GaussSystem));
    if(result == NULL) {
        exit(-1);
    }

    result->A = A;
    result->p = p;
    result->b = b;
    result->x = x;
    result->rank = baseVarsNum;
    result->baseVars = baseVars;
    result->isSolvable = isSolvable;
    result->solveTime = solveTime;

    return result;
}

DTYPE productRowCol(MyzMatrix *A, MyzMatrix *B, int i, int j) {
    DTYPE sum;
    int k;

    assert(matN(A) == matM(B));

    sum = 0.0;
    for(k=0; k < matN(A); k++) {
        sum += at(A, i, k) * at(B, k, j);
    }

    return sum;
}

int setMatMulti(MyzMatrix *A, MyzMatrix *B, MyzMatrix *C) {
    int i, j;

    assert( (matN(A) == matM(B)) && (matM(A) == matM(C)) && (matN(B) == matN(C)) );

    for(i=0; i < matM(A); i++) {
        for(j=0; j < matN(B); j++) {
            set(C, i, j, productRowCol(A, B, i, j));
        }
    }

    return 0;
}

int setScalarMulti(DTYPE val, MyzMatrix *A) {
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(A, i, j, val * at(A, i, j));
        }
    }
    return 0;
}

int setMatCopy(MyzMatrix *A, MyzMatrix *C) {
    int i, j;

    assert( (matN(A) == matN(C)) && (matM(A) == matM(C)) );

    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(C, i, j, at(A, i, j));
        }
    }
    return 0;
}

MyzMatrix *matCopy(MyzMatrix *A) {
    MyzMatrix *C;
    if(A->isColMode) {
        C = newColAccessMyzMatrix(matM(A), matN(A));
    }
    else {
        C = newMyzMatrix(matM(A), matN(A));
    }

    setMatCopy(A, C);

    return C;
}


DTYPE matDiff1(MyzMatrix *A, MyzMatrix *B) {
    DTYPE err;
    int i, j;

    err = 0.0;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            err += fabs(at(A, i, j) - at(B, i, j));
        }
    }

    return err;
}

DTYPE sumOfCol(MyzMatrix *A, int j) {
    DTYPE sum;
    int i;

    sum =  0.0;
    for(i=0; i<matM(A); i++) {
        sum += at(A, i, j);
    }
    return sum;
}

void matNormalizeCol1(MyzMatrix *A) {
    DTYPE sum;
    int j, i;

    for(j=0; j<matN(A); j++) {
        sum = sumOfCol(A, j);
        for(i=0; i<matM(A); i++) {
            set(A, i, j, at(A, i, j)/sum);
        }
    }
}

MyzMatrix *powerMethod(MyzMatrix *A, int itrMin, int itrMax) {
    MyzMatrix *x, *preX;
    DTYPE lambda = 1.0;
    int count;

    //x = newMyzMatrix(matM(A), 1);
    x = newColAccessMyzMatrix(matM(A), 1);
    matFill(x, 1.0);
    preX = matCopy(x);

    count = 0;
    while(1==1) {
        if(count > itrMax) break;

        setMatCopy(x, preX);
        setMatMulti(A, preX, x);
        lambda = at(x, 0, 0) / at(preX, 0, 0);
        matNormalizeCol1(x);

        if((count>itrMin) && isZero(matDiff1(x, preX))) break;
        count++;
    }

    printf("lambda: %f, itr: %d\n", lambda, count);

    freeMyzMatrix(preX);

    matNormalizeCol1(x);

    return x;
}

DTYPE a58[]= {
    1, 3, 3, 2,
    2, 6, 9, 5,
    -1, -3, 3, 0
};

DTYPE b58[] = {
    1, 2, -1
};

DTYPE p1[] = {
    87,  270, -12,  -49, -276,  40,
    -14, -45,   6,  10,   46,  -4,
    -50, -156,  4,  25,  162, -25,
    94,   294, -5,  -47, -306, 49,
    1,    1,  3,   1,   0,   2,
    16,   48,  1,  -6,  -48,  8
}; //6x6

DTYPE p72C1p[] = {
    1, 5, 9,
    1.0/5.0, 1, 3,
    1.0/9.0, 1.0/3.0, 1,
    1.0/5.0, 1, 3
}; //3x3

DTYPE p72C1[] = {
    1, 5, 9, 5,
    1.0/5.0, 1, 3, 1,
    1.0/9.0, 1.0/3.0, 1, 1.0/2.0,
    1.0/5.0, 1, 3, 1
}; //4x4

DTYPE p72C2[] = {
    1, 1.0/7.0, 1.0/3.0, 1.0/7.0,
    7, 1, 3, 1,
    3, 1.0/3.0, 1, 1.0/3.0,
    7, 1, 3, 1
}; //4x4

void testMulti(int m, int k, int n) {
    MyzMatrix *A = newMyzMatrix(m, k);
    MyzMatrix *B = newMyzMatrix(k, n);
    MyzMatrix *Bt = newColAccessMyzMatrix(k, n);
    MyzMatrix *AB = newMyzMatrix(m, n);

    time_t start_t, end_t;
    int seed;
    DTYPE r=0.01;
    int itr;

    for(itr=0; itr<3; itr++) {
        seed = 100+itr*10;
        r *= 100;
        printf("seed:%d, rMax:%f\n", seed, r);

        setRandVal(A, seed, -r, r);
        setRandVal(B, seed+10, -r, r);
        setRandVal(Bt, seed+20, -r, r);

        start_t = clock();
        setMatMulti(A, B, AB);
        end_t = clock();
        printf("time=%f,  %d %d %d\n", (double)(end_t - start_t)/CLOCKS_PER_SEC, m, k, n);

        start_t = clock();
        setMatMulti(A, Bt, AB);
        end_t = clock();
        printf("time=%f,  %d %d %d  (ColAccessMode)\n", (double)(end_t - start_t)/CLOCKS_PER_SEC, m, k, n);
    }

    freeMyzMatrix(A);
    freeMyzMatrix(B);
    freeMyzMatrix(Bt);
    freeMyzMatrix(AB);
}

int testGauss(MyzMatrix *aA, MyzMatrix *ab) {
    GaussSystem *gs = gauss2(aA, ab);
    MyzMatrix *tmpB = matCopy(ab);

    if(gs->isSolvable && matM(gs->A)<10) {
        printf("A\n"); stdoutMyzMatrix(aA);
        printf("b\n"); stdoutMyzMatrix(ab);
        printf("x\n"); stdoutMyzMatrix(gs->x);
    }

    if(gs->isSolvable) {
        printf("solvable!,  ");
    }
    else {
        printf("NOT solvable,  ");
    }

    printf("Rank is %d,  ", gs->rank);
    printf("solve time: %fsec,  ", gs->solveTime);

    setMatMulti(aA, gs->x, tmpB);
    printf("diff %15.10f\n", matDiff1(ab, tmpB));

    freeMyzMatrix(tmpB);

    freeGaussSystem(gs);

    return 1;
}

int testPower(int m, int itrMin, int itrMax) {
    MyzMatrix *A = newMyzMatrix(m, m);
    MyzMatrix *x;

    time_t start_t, end_t;
    int seed;
    DTYPE r=0.01;
    int itr;
    DTYPE diff;
    MyzMatrix *tmpX;

    for(itr=0; itr<3; itr++) {
        seed = 1000+itr*10;
        r *= 100;
        printf("power: seed:%d, rMax:%f\n", seed, r);

        setRandVal(A, seed, -r, r);

        start_t = clock();
        x = powerMethod(A, itrMin, itrMax);
        end_t = clock();

        tmpX = matCopy(x);
        setMatMulti(A, x, tmpX);
        matNormalizeCol1(tmpX);
        diff = matDiff1(x, tmpX);

        printf("time=%f,  diff=%15.13f, m=%d\n", (double)(end_t - start_t)/CLOCKS_PER_SEC, diff, m);
    }

    freeMyzMatrix(A);
    freeMyzMatrix(x);
    freeMyzMatrix(tmpX);

    return 1;
}


int main(void)
{
    MyzMatrix *A, *b;
    int M, N;

    testMulti(200, 500, 200);

    A = newMyzMatrixFromArray(a58, 3, 4);
    b = newMyzMatrixFromArray(b58,  3, 1);
    testGauss(A, b);
    freeMyzMatrix(A);
    freeMyzMatrix(b);

    for(M=100; M<=105; M++) {

        N = M+10;
        A = newRandMyzMatrix(1, -10, 10, M, N);
        b = newRandMyzMatrix(2, -10, 10, M, 1);

        testGauss(A, b);

        freeMyzMatrix(A);
        freeMyzMatrix(b);
    }

    A = newMyzMatrixFromArray(p1, 3, 3);
    b = powerMethod(A, 0, 100);
    stdoutMyzMatrix(A);
    stdoutMyzMatrix(b);
    freeMyzMatrix(A);
    freeMyzMatrix(b);

    for(M=1000; M<=1001; M++) {
        A = newRandMyzMatrix(1, -10, 10, M, N);
        testPower(M, 0, 100);
        freeMyzMatrix(A);
    }

    return 0;
}
