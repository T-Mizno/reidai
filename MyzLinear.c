#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <assert.h>

#include <time.h>


#define DTYPE double


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

struct _GaussResult
{
    int *flgBaseJs;
    int *baseJs;
    MyzMatrix *x;
    MyzMatrix *xs;
    int numOfXs;
};
typedef struct _GaussResult GaussResult;



int matM(MyzMatrix *A);
int matN(MyzMatrix *A);

DTYPE at(MyzMatrix *A, int i, int j);
int set(MyzMatrix *A, int i, int j, DTYPE val);

MyzMatrix *matCopy(MyzMatrix *A);
DTYPE matDiff1(MyzMatrix *A, MyzMatrix *B);


DTYPE epsillon = 10e-15;

int almostEqual(DTYPE val1, DTYPE val2)
{
    return (fabs(val1 - val2) < epsillon) ;
}

int almostZero(DTYPE val)
{
    return almostEqual(val, 0.0);
}

int *newVectorI(int M)
{
    int *r;
    r = (int *) malloc(sizeof(int) * M);
    if(r == NULL) {
        printf("Cannot create an int vector.\n");
        exit(-1);
    }

    return r;
}

void matFill(MyzMatrix *A, DTYPE val)
{
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(A, i, j, val);
        }
    }
    return;
}

MyzMatrix *newMyzMatrix(int M, int N)
{
    MyzMatrix *Mat;
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
    {
        int i;
        for(i=0; i<M; i++) {
            Mat->_entity[i] = (DTYPE *) malloc(sizeof(DTYPE) * N);
            if(Mat->_entity[i] == NULL) {
                printf("Cannot create row %d.\n", i);
                for(;i>=0; i--) free(Mat->_entity[i]);
                free(Mat);
                exit(-1);
            }
        }
    }
    Mat->_M = M;
    Mat->_N = N;

    Mat->isColMode = (1 != 1); // false

    //matFill(Mat, 0.0);

    return Mat;
}

MyzMatrix *newRandMyzMatrix(int seed, DTYPE rMin, DTYPE rMax, int m, int n)
{
    MyzMatrix *mat = newMyzMatrix(m, n);
    int i,j;

    srand(seed);
    for(i=0; i<matM(mat); i++) {
        for(j=0; j<matN(mat); j++) {
            set(mat, i, j, (rMax-rMin)*((DTYPE)rand()/(DTYPE)RAND_MAX)+rMin);
        }
    }
    return mat;
}

MyzMatrix *newColAccessMyzMatrix(int M, int N)
{
    MyzMatrix *mat;

    mat = newMyzMatrix(N, M);
    mat->isColMode = (1 == 1); // true
    mat->_M = M;
    mat->_N = N;

    return mat;
}

int matM(MyzMatrix *A)
{
    return A->_M;
}
int matN(MyzMatrix *A)
{
    return A->_N;
}

void freeMyzMatrix(MyzMatrix *A)
{
    int i;

    if(A->isColMode) {
        A->_M = A->_N;
    }


    for(i=0; i<matM(A); i++) {
        free(A->_entity[i]);
    }
    free(A->_entity);
    free(A);
}

DTYPE at(MyzMatrix *A, int i, int j)
{
    assert( (i >= 0) && (i < matM(A)) );
    assert( (j >= 0) && (j < matN(A)) );

    if(A->isColMode) {
        return A->_entity[j][i];
    }

    return A->_entity[i][j];
}

int set(MyzMatrix *A, int i, int j, DTYPE val)
{
    assert( (i >= 0) && (i < matM(A)) );
    assert( (j >= 0) && (j < matN(A)) );

    if(A->isColMode) {
        A->_entity[j][i] = val;
        return 0;
    }

    A->_entity[i][j] = val;
    return 0;
}

void stdoutMyzMatrix(MyzMatrix *A)
{
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            //printf(" %5.3f", at(A, i,j));
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

void stdoutPMatrix(int *P, MyzMatrix *A)
{
    int i, j;
    for(i=0; i<matM(A); i++) {
        printf("P[%2d]=%2d: ", i, P[i]);
        for(j=0; j<matN(A); j++) {
            printf(" %5.3f", at(A, P[i], j));
        }
        printf("\n");
    }
    return;
}

/*
GaussSytem *newGaussSystem(MyzMatrix *aA, MyzMatrix *ab, int rank) {
    GaussSystem
}
*/

GaussResult *newGaussResult(int M, int N)
{
    GaussResult *result = NULL;
    int j;

    result = (GaussResult *) malloc (sizeof(GaussResult));
    if(result == NULL) {
        exit(-1);
    }

    result->flgBaseJs = newVectorI(N);

    for(j=0; j<N; j++) {
        result->flgBaseJs[j] = (1 != 1);
    }

    result->baseJs = NULL;

    //result->x = newColAccessMyzMatrix(N, 1);
    result->x = newMyzMatrix(N, 1);
    result->xs = NULL;

    result->numOfXs = 0;
    return result;
}

void freeGaussSystem(GaussSystem *gs) {
    if(gs->A != NULL) freeMyzMatrix(gs->A);
    if(gs->b != NULL) freeMyzMatrix(gs->b);
    if(gs->p != NULL) free(gs->p);
    if(gs->x != NULL) freeMyzMatrix(gs->x);
    if(gs->baseVars != NULL) free(gs->baseVars);
    free(gs);
}

void freeGaussResult(GaussResult *result)
{
    if(result->flgBaseJs != NULL) free(result->flgBaseJs);
    if(result->baseJs != NULL) free(result->baseJs);
    if(result->x != NULL) freeMyzMatrix(result->x);
    if(result->xs != NULL) freeMyzMatrix(result->xs);
    free(result);
    return;
}

void stdoutGaussResult(GaussResult *result, int M, int N)
{
    int j;
    printf("flgBaseJs: ");
    for(j=0; j<N; j++) {
        printf(" [%d", j);
        if(result->flgBaseJs[j]) {
            printf(":b");
        }
        else {
            printf(":f");
        }
        printf("]");
    }
    printf("\n");

    printf("baseJs: ");
    for(j=0; j < N - result->numOfXs; j++) {
        printf(" %d", result->baseJs[j]);
    }
    printf("\n");

    printf("x:\n");
    stdoutMyzMatrix(result->x);
    printf("numOfBaseJs: %d\n", (N - result->numOfXs));
    if(result->numOfXs > 0) {
        printf("xs:\n");
        stdoutMyzMatrix(result->xs);
    }
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


int backward(int *P, MyzMatrix *A, MyzMatrix *b, MyzMatrix *x, int xsI, GaussResult *r)
{
    int i, j;

    //stdoutGaussResult(r, matM(A), matN(A));
    //printf("go");

    for(i=(matN(A) - r->numOfXs) -1; i>=0; i--) {
        double tmpb = 0.0;
        for(j=r->baseJs[i]+1; j<matN(A); j++) {
            tmpb += at(A, P[i], j) * at(x, j, xsI);
            //printf("r->baseJs[j] %d, xsi %d\n", r->baseJs[j], xsI);
        }
        set(x, r->baseJs[i], xsI, (at(b, P[i], 0) - tmpb) / at(A, P[i], r->baseJs[i]));

        //        set(x, r->baseJs[j], xsI, 2.0);
    }

    return 0;
}


int gauss(int *P, MyzMatrix *A, MyzMatrix *b, GaussResult *r)
{
    int pivotI, pivotJ;
    int i, j;
    DTYPE pivot;
    int M = matM(A);
    int N = matN(A);

    pivotI = 0;
    pivotJ = 0;

    while(1 == 1) {
        if(pivotI > M-1) break;
        while(1==1) {
            if(pivotJ > N-1) break;
            searchMaxIAndSwapI(P, A, pivotI, pivotJ);
            if(! almostZero(at(A, P[pivotI], pivotJ))) break;
            pivotJ++ ;
        }

        if(pivotJ > N-1) break;

        r->flgBaseJs[pivotJ] = (1 == 1);

        pivot = at(A, P[pivotI], pivotJ);
        for(i=pivotI+1; i<M; i++) {
            DTYPE rPivot = at(A, P[i], pivotJ) / pivot;
            set(A, P[i], pivotJ,  rPivot);
            for(j=pivotJ+1; j<N; j++) {
                DTYPE tmpV = at(A, P[i], j) - at(A, P[i], pivotJ) * at(A, P[pivotI], j);
                set(A, P[i], j, tmpV);
            }
            set(b, P[i], 0, at(b, P[i], 0) - at(A, P[i], pivotJ) * at(b, P[pivotI], 0));
        }


        /*
        printf("pivotI=%d, pivotJ=%d\n", pivotI, pivotJ);
        stdoutPMatrix(P, A);
        stdoutPMatrix(P, b);
        */

        pivotI++;
        pivotJ++;
    }


    // numOfXs
    r->numOfXs = N;
    for(j=0; j<N; j++) {
        if(r->flgBaseJs[j]) r->numOfXs--;
    }

    // baseJs
    r->baseJs = newVectorI(N - r->numOfXs);
    {
        int baseCount = 0;
        for(j=0; j<N; j++){
            if(r->flgBaseJs[j]) {
                r->baseJs[baseCount] = j;
                baseCount++;
            }
        }
    }


    r->xs = newColAccessMyzMatrix(N, r->numOfXs);
    for(i=0; i<r->numOfXs; i++) {
        for(j=0; j<N; j++) {
            set(r->xs, j, i, 0.0);
        }
    }
    {
        int freeVarCount = 0;
        for(j=0; j<N; j++){
            if(! r->flgBaseJs[j]) {
                set(r->xs, j, freeVarCount, 1.0);
                freeVarCount++;
            }
        }
    }

    matFill(r->x, 0.0);
    backward(P, A, b, r->x, 0, r);

    // xs
    {
        MyzMatrix *zerob;
        zerob = newMyzMatrix(M, 1);
        for(i=0; i<M; i++) {
            set(zerob, i, 0, 0.0);
        }
        for(i=0; i<r->numOfXs; i++) {
            backward(P, A, zerob, r->xs, i, r);
        }
        freeMyzMatrix(zerob);
    }
    return 0;

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
        if(almostZero(at(A, p[pivotI], pivotJ))) {
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
        isSolvable = isSolvable && almostZero(at(b, p[i], 0)/((DTYPE)matN(A)));
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

DTYPE productRowCol(MyzMatrix *A, MyzMatrix *B, int i, int j)
{
    DTYPE sum;
    int k;

    assert(matN(A) == matM(B));

    sum = 0.0;
    for(k=0; k < matN(A); k++) {
        sum += at(A, i, k) * at(B, k, j);
    }

    return sum;
}

int setMatMulti(MyzMatrix *A, MyzMatrix *B, MyzMatrix *C)
{
    int i, j;

    assert( (matN(A) == matM(B)) && (matM(A) == matM(C)) && (matN(B) == matN(C)) );

    for(i=0; i < matM(A); i++) {
        for(j=0; j < matN(B); j++) {
            set(C, i, j, productRowCol(A, B, i, j));
        }
    }

    return 0;
}

int setScalarMulti(DTYPE val, MyzMatrix *A)
{
    int i, j;
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(A, i, j, val * at(A, i, j));
        }
    }
    return 0;
}

int setMatCopy(MyzMatrix *A, MyzMatrix *C)
{
    int i, j;

    assert( (matN(A) == matN(C)) && (matM(A) == matM(C)) );

    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            set(C, i, j, at(A, i, j));
        }
    }
    return 0;
}

MyzMatrix *matCopy(MyzMatrix *A)
{
    MyzMatrix *C;
    C = newMyzMatrix(matM(A), matN(A));
    setMatCopy(A, C);

    return C;
}

MyzMatrix *matCopyColAccess(MyzMatrix *A)
{
    MyzMatrix *C;
    C = newColAccessMyzMatrix(matM(A), matN(A));
    setMatCopy(A, C);

    return C;
}


DTYPE errorX(MyzMatrix *A, MyzMatrix *xs, int xsI, MyzMatrix *b)
{
    int i, j;
    DTYPE tmp;
    int M = matM(A);
    int N = matN(A);

    DTYPE error = 0.0;

    for(i=0; i<M; i++) {
        tmp = 0.0;
        for(j=0; j<N; j++) {
            tmp += at(A, i, j) * at(xs, j, xsI);
        }
        error += fabs(at(b, i, 0)-tmp);
    }
    return error;
}

DTYPE errorXs(MyzMatrix *A, GaussResult *r)
{
    MyzMatrix *b = newMyzMatrix(matM(A), 1);
    DTYPE error = 0.0;
    int i;

    for(i=0; i<matM(A); i++) {
        set(b, i, 0, 0.0);
    }

    for(i=0; i < r->numOfXs; i++) {
        error += errorX(A, r->xs, i, b);
    }
    return error;
}

int testFrame2(MyzMatrix *aA, MyzMatrix *ab) {
    GaussSystem *gs = gauss2(aA, ab);
    MyzMatrix *tmpB = matCopy(ab);

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

MyzMatrix *newMyzMatrixFromArray(DTYPE a[], int M, int N)
{
    int i, j;
    MyzMatrix *C = newMyzMatrix(M, N);

    for(i=0; i<M; i++) {
        for(j=0; j<N; j++) {
            set(C, i, j, a[i*N + j]);
        }
    }

    return C;
}

void countSum(MyzMatrix *A)
{
    DTYPE sum;
    time_t start_t, end_t;
    int i, j;

    sum = 0.0;

    start_t = clock();
    /*
    for(i=0; i<matM(A); i++) {
        for(j=0; j<matN(A); j++) {
            sum += at(A, i, j);
        }
    }
    */
    for(j=0; j<matN(A); j++) {
        for(i=0; i<matM(A); i++) {
            sum += at(A, i, j);
        }
    }
    end_t = clock();

    printf("time=%f,  %f\n", (double)(end_t - start_t)/CLOCKS_PER_SEC, sum);
}


void countMulti(MyzMatrix *A, MyzMatrix *B, MyzMatrix *C)
{
    time_t start_t, end_t;

    start_t = clock();
    setMatMulti(A, B, C);
    end_t = clock();

    printf("time=%f,  %d %d\n", (double)(end_t - start_t)/CLOCKS_PER_SEC, matM(C), matN(C));
}


DTYPE matDiff1(MyzMatrix *A, MyzMatrix *B)
{
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

DTYPE sumOfCol(MyzMatrix *A, int j)
{
    DTYPE sum;
    int i;

    sum =  0.0;
    for(i=0; i<matM(A); i++) {
        sum += at(A, i, j);
    }
    return sum;
}

void matNormalizeCol1(MyzMatrix *A)
{
    DTYPE sum;
    int j, i;

    for(j=0; j<matN(A); j++) {
        sum = sumOfCol(A, j);
        for(i=0; i<matM(A); i++) {
            set(A, i, j, at(A, i, j)/sum);
        }
    }
}

MyzMatrix *powerMethod(MyzMatrix *A, int itrMax)
{
    MyzMatrix *x, *preX;
    DTYPE lambda = 1.0;
    int count;

    x = newMyzMatrix(matM(A), 1);
    matFill(x, 0);
    preX = matCopy(x);
    matFill(preX, 1.0);

    count = 0;
    do {
        matNormalizeCol1(preX);
        setMatMulti(A, preX, x);
        lambda = at(x, 0, 0) / at(preX, 0, 0);
        setScalarMulti(1.0/lambda, x);
        // swap
        {
            MyzMatrix *tmp;
            tmp = preX;
            preX = x;
            x = tmp;
        }
        count++;
    }
    while ( (! almostZero(matDiff1(x, preX))) && (count <itrMax) );

    printf("lambda: %f, itr: %d\n", lambda, count);

    freeMyzMatrix(preX);

    matNormalizeCol1(x);

    return x;

}


DTYPE a58[]= {1, 3, 3, 2,
              2, 6, 9, 5,
              -1, -3, 3, 0};
DTYPE b58[] = {1, 2, -1};

DTYPE p1[] = { 87,  270, -12,  -49, -276,  40,
               -14, -45,   6,  10,   46,  -4,
               -50, -156,  4,  25,  162, -25,
               94,   294, -5,  -47, -306, 49,
               1,    1,  3,   1,   0,   2,
               16,   48,  1,  -6,  -48,  8}; //6x6

DTYPE p72C1p[] = { 1, 5, 9,
                  1.0/5.0, 1, 3,
                  1.0/9.0, 1.0/3.0, 1,
                  1.0/5.0, 1, 3}; //3x3
DTYPE p72C1[] = { 1, 5, 9, 5,
                  1.0/5.0, 1, 3, 1,
                  1.0/9.0, 1.0/3.0, 1, 1.0/2.0,
                  1.0/5.0, 1, 3, 1}; //4x4
DTYPE p72C2[] = { 1, 1.0/7.0, 1.0/3.0, 1.0/7.0,
                  7, 1, 3, 1,
                  3, 1.0/3.0, 1, 1.0/3.0,
                  7, 1, 3, 1}; //4x4

int main(void)
{
    MyzMatrix *A, *b;
    int M, N;

    //test2();

    for(M=1000; M<=1005; M++) {
        {
            int i, j;
            N = M+10;
            A = newRandMyzMatrix(1, -10, 10, M, N);
            b = newRandMyzMatrix(2, -10, 10, M, 1);
        }
        testFrame2(A, b);

        freeMyzMatrix(A);
        freeMyzMatrix(b);
    }


    /*
    A = newMyzMatrixFromArray(a58, 3, 4);
    b = newMyzMatrixFromArray(b58,  3, 1);
    testFrame2(A, b);

    freeMyzMatrix(A);
    freeMyzMatrix(b);
    */

    /*
    A = newMyzMatrixFromArray(p72C1p, 3, 3);
    b = powerMethod(A, 100);

    stdoutMyzMatrix(A);
    stdoutMyzMatrix(b);

    freeMyzMatrix(A);
    */

    return 0;
}
