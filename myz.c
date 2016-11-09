#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "myz.h"

void _MyzError(char *filename, unsigned lineNum, char *message)
{
    fflush(stdout);
    fprintf(stderr, "\nError ! :  File %s, Line %d\n", filename, lineNum);
    fprintf(stderr, "Messages : \" %s \"\n\n", message);
    fprintf(stderr, "\n\nProgram will be abort with status 0.\n\n");
    fflush(stderr);
    exit(0);
}

//////////////////////////////////////////////////////////////////
// create Array and Matrix
//////////////////////////////////////////////////////////////////
/************************************************************
 ************************************************************
 * For Vector
 ************************************************************
 ************************************************************/

/*************************************************
 * make Vector
 *************************************************/
#define NEW_VECTOR( SUFIX, TYPE, INITIAL_VALUE )            \
                                                            \
    TYPE *newVector##SUFIX ( int size )                     \
    {                                                       \
        TYPE *array;                                        \
        int i;                                              \
                                                            \
        assert( size >= 0 );                                \
                                                            \
        array = ( TYPE *)malloc(sizeof( TYPE ) * (size+1)); \
        if(array == NULL)myzError("Memory is Lack!");       \
        ForEach(i, 0, size)array[i] = INITIAL_VALUE ;       \
                                                            \
        return array;                                       \
    }                                                       \
                                                            \

NEW_VECTOR(I, int,       i)
NEW_VECTOR(D, double,  0.0)
NEW_VECTOR(F, float,   0.0)
NEW_VECTOR(C, char,   '\0')

/****************************************************
 * Set Vector
 ****************************************************/
#define SET_VECTOR( SUFIX , TYPE )                              \
                                                                \
    void setVector##SUFIX( TYPE *origin, TYPE *copy, int dim)   \
    {                                                           \
     int i;                                                     \
                                                                \
     assert( origin != NULL );                                  \
     assert( (origin + sizeof( TYPE ) * dim) != NULL);          \
     assert( copy   != NULL );                                  \
     assert( copy    + sizeof( TYPE ) * dim  != NULL );         \
                                                                \
     ForEach(i, 0, dim-1) copy[i] = origin[i];                  \
     return;                                                    \
     }                                                          \
                                                                \

SET_VECTOR( D, double )
SET_VECTOR( F, float  )
SET_VECTOR( I, int    )
SET_VECTOR( C, char   )



/*********************************************
 * Create vector clone
 *********************************************/
#define CREATE_VECTOR_CLONE( SUFIX , TYPE )                 \
    TYPE *createVectorClone##SUFIX( TYPE *origin, int size) \
    {                                                       \
        TYPE *clone;                                        \
                                                            \
        assert( origin != NULL );                           \
        assert( origin + sizeof( TYPE ) * size != NULL );   \
                                                            \
        clone = newVector##SUFIX( size );                   \
        setVector##SUFIX(origin, clone, size);              \
                                                            \
        return clone;                                       \
    }                                                       \
                                                            \

CREATE_VECTOR_CLONE( D, double )
CREATE_VECTOR_CLONE( F, float  )
CREATE_VECTOR_CLONE( I, int    )
CREATE_VECTOR_CLONE( C, char   )




/**************************************************************
 **************************************************************
 * For Matrix
 **************************************************************
 **************************************************************/

/******************************************************
 * New Matrix
 ******************************************************/
/** internal Create matrix  **/
#define __NEW_MATRIX( SUFIX , TYPE )                            \
                                                                \
    TYPE  **_newMatrix##SUFIX (int M, int N)                    \
    {                                                           \
     int i;                                                     \
     TYPE  **matrix;                                            \
                                                                \
     assert(M >= 0);                                            \
     assert(N >= 0);                                            \
                                                                \
     matrix = ( TYPE **)malloc((M+1) * sizeof( TYPE *));        \
     if (matrix == NULL)                                        \
         {                                                      \
          myzError("Memory is Lack!");                          \
          return NULL;                                          \
          }                                                     \
     matrix[M] = NULL;                                          \
     for (i = 0; i < M; i++)                                    \
         {                                                      \
          matrix[i] = ( TYPE *)malloc(sizeof( TYPE ) * (N+1));  \
          if (matrix[i] == NULL)                                \
              {                                                 \
               while (--i >= 0) free(matrix[i]);                \
               free(matrix);                                    \
               myzError("Memory is Lack!");                     \
               return NULL;                                     \
               }                                                \
          }                                                     \
     return matrix;                                             \
     }                                                          \
                                                                \

__NEW_MATRIX( D , double )
__NEW_MATRIX( F , float  )
__NEW_MATRIX( I , int    )
__NEW_MATRIX( C , char   )


#define NEW_MATRIX( SUFIX , TYPE )                          \
                                                            \
    TYPE **newMatrix##SUFIX (int rowSize, int colSize)      \
    {                                                       \
        TYPE **matrix;                                      \
                                                            \
        assert(rowSize >= 0);                               \
        assert(colSize >= 0);                               \
                                                            \
        matrix = _newMatrix##SUFIX (rowSize, colSize);      \
        if (matrix == NULL) myzError("Memory is Lack!");    \
                                                            \
        return matrix;                                      \
    }                                                       \
                                                            \

NEW_MATRIX( D , double )
NEW_MATRIX( F , float  )
NEW_MATRIX( I , int    )
NEW_MATRIX( C , char   )



/*********************************************************
 * Read matrix from File
 *********************************************************/

/** Skip data in File **/
#define __SKIP_DATA_OF_FILE( SUFIX , TYPE , FORMAT )        \
                                                            \
    TYPE _skipDataOfFile##SUFIX(FILE *fp, int skipSize)     \
    {                                                       \
     int i;                                                 \
     TYPE tmp;                                              \
                                                            \
     assert(fp != NULL);                                    \
                                                            \
     ForEach(i, 0, skipSize-1)                              \
     if(EOF == fscanf(fp, FORMAT, &tmp))return MYZ_FALSE;   \
     return MYZ_TRUE;                                       \
     }                                                      \
                                                            \

__SKIP_DATA_OF_FILE( D , double , "%lf")
__SKIP_DATA_OF_FILE( F , float  , "%f" )
__SKIP_DATA_OF_FILE( I , int    , "%d" )
__SKIP_DATA_OF_FILE( C , char   , "%c")

/**********************************************************
 * read double data from file
 * if reading data is fail then return -1;
 * else return dataNum;
 **********************************************************/
#define READ_MATRIX_FROM_FILE( SUFIX , TYPE , FORMAT )              \
                                                                    \
    int readMatrixFromFile##SUFIX                                   \
    (char *filename, TYPE ***data, int colSize)                     \
    {                                                               \
        FILE *fp;                                                   \
        TYPE **matrix;                                              \
        int rowSize;                                                \
                                                                    \
        int i,j;                                                    \
                                                                    \
        assert( filename != NULL );                                 \
        assert( data    != NULL );                                  \
        assert( colSize  >= 0    );                                 \
                                                                    \
        rowSize = MYZ_FALSE;                                        \
        if(NULL == (fp = fopen(filename, "r")))                     \
            myzError("File cannot Open!");                          \
                                                                    \
        for(rowSize=1;                                              \
            _skipDataOfFile##SUFIX (fp, colSize);                   \
            rowSize++);                                             \
        rowSize--;                                                  \
                                                                    \
        fseek(fp, 0, SEEK_SET);                                     \
        matrix = newMatrix##SUFIX (rowSize, colSize);               \
                                                                    \
        ForEach(i, 0, rowSize-1)                                    \
            {                                                       \
                ForEach(j, 0, colSize-1)                            \
                    if(EOF == fscanf(fp, FORMAT, &matrix[i][j]))    \
                        myzError("File access error!");             \
            }                                                       \
        fclose(fp);                                                 \
                                                                    \
        *data = matrix;                                             \
        return rowSize;                                             \
    }                                                               \
                                                                    \

READ_MATRIX_FROM_FILE( D , double , "%lf" )
READ_MATRIX_FROM_FILE( F , float  , "%f" )
READ_MATRIX_FROM_FILE( I , int    , "%d" )
READ_MATRIX_FROM_FILE( C , char   , "%c" )


/**********************************************************
 * Free matrix
 **********************************************************/
#define FREE_MATRIX( SUFIX , TYPE )             \
                                                \
    void freeMatrix##SUFIX( TYPE **matrix )     \
    {                                           \
     TYPE **tmp;                                \
                                                \
     assert(matrix != NULL );                   \
                                                \
     tmp = matrix;                              \
     while (*tmp != NULL) free(*tmp++);         \
     free(matrix);                              \
     }                                          \
                                                \

FREE_MATRIX( D , double )
FREE_MATRIX( F , float  )
FREE_MATRIX( I , int    )
FREE_MATRIX( C , char   )


int trimZeroRowD(double **Origin, int originRowSize, int colSize)
{
    int size;
    double **T;

    double tmp;
    int i,j;

    T = newMatrixD(originRowSize, colSize);
    size = 0;
    ForEach(i, 0, originRowSize-1)
        {
            tmp = 0.0;
            ForEach(j, 0, colSize-1) tmp += Origin[i][j];
            if(tmp !=  0.0)
                {
                    ForEach(j, 0, colSize-1)T[size][j] = Origin[i][j];
                    size++;
                }
        }
    ForEach(i, 0, size-1)
        ForEach(j, 0, colSize-1)
        Origin[i][j] = T[i][j];
    freeMatrixD(T);
    return size;
}

void multiMatrixVectorD(double **A, double *b, double *result, int M, int N)
{
    int i,j;

    ForEach(i, 0, M-1)
        {
            result[i] = 0.0;
            ForEach(j, 0, N-1)
                {
                    result[i] += A[i][j] * b[j];
                }
        }
    return;
}
  
double **multiMatrixMatrixD(double **A, double **B, int M, int N)
{
    double **R;
    int i, j, k;

    R = newMatrixD(M, M);
    ForEach(i, 0, M-1)
        {
            ForEach(j, 0, M-1)
                {
                    R[i][j] = 0.0;
                    ForEach(k, 0, N-1)R[i][j] += A[i][k] * B[k][j];
                }
        }
    return R;
}

int setMatrixD(double **form, double **to, int M, int N)
{
    int i,j;

    ForEach(i, 0, M-1)
        {
            ForEach(j, 0, N-1)
                {
                    to[i][j] = form[i][j];
                }
        }
    return 0;
}
double **createMatrixCloneD(double **Ori, int M, int N)
{
    double **C;
    C = newMatrixD(M, N);
    setMatrixD(Ori, C, M, N);
    return C;
}


/**
 *  Combination
 **/
int *initCombination(int n)
{
    int i;
    int *C;

    C = newVectorI(n + 2);
    for(i=0; i<=n; i++)C[i] = i;
    return C;
}
int nextCombination(int *r, int n, int k)
{
    int i, j;
    int *tmp;

    if((k < 0) || (k > n)) return MYZ_FALSE;

    tmp = newVectorI(n+2);
    tmp[0] = 0;
    ForEach(i, 0, n)tmp[i+1] = r[i]+1;

    i=k;
    while(i>0 && tmp[i] == n - k+i) i--;
    if(i==0)
        {
            free(tmp);
            return MYZ_FALSE;
        }

    tmp[i]++;
    for(j=i+1; j<=k; j++) tmp[j] = tmp[j-1] +1;

    ForEach(i, 0, n)r[i] = tmp[i+1] - 1;
    free(tmp);
    return MYZ_TRUE;
}
