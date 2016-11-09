#ifndef __MYZ_H__
#define __MYZ_H__

// for boolean type
typedef int myzBoolean;
#define MYZ_TRUE (1 == 1)
#define MYZ_FALSE (! MYZ_TRUE)

// for 'ForEach'
#ifndef ForEach
#define ForEach(var, start, end)  for(var=start; var<=end; var++)
#endif //ForEach

#ifndef rForEach
#define rForEach(var, start, end)  for(var=start; var>=end; var--)
#endif //rForEach

//////////////////////////////////////////////////////////////
// Prototype
//////////////////////////////////////////////////////////////
void _MyzError(char *filename, unsigned line, char *message);
#define myzError(m) _MyzError(__FILE__, __LINE__, m)

/**
 * Vector
 **/
   int *newVectorI(int n);
double *newVectorD(int n);
  char *newVectorC(int n);
   int *createVectorCloneI(int o[], int n);
double *createVectorCloneD(double o[], int n);
  void setVectorI(int origin[], int copy[], int dim);
  void setVectorD(double origin[], double copy[], int dim);

/**
 * Matrix
 **/
   int **newMatrixI(int nrow, int ncol);
double **newMatrixD(int nrow, int ncol);
  char **newMatrixC(int nrow, int ncol);
   int skipDataOfFileI(FILE *fp, int m);
   int skipDataOfFileD(FILE *fp, int m);
   int readMatrixFromFileD(char *filename, double ***data, int colSize);
   int readMatrixFromFileI(char *filename,    int ***data, int colSize);
  void freeMatrixI(int    **a);
  void freeMatrixD(double **a);
   int trimZeroRowD(double **Origin, int originRowSize, int colSize);
  void multiMatrixVectorD(double **A, double *b, double *result, int M, int N);
double **multiMatrixMatrixD(double **A, double **B, int M, int N);
   int setMatrixD(double **form, double **to, int M, int N);
double **createMatrixCloneD(double **Ori, int M, int N);



int *initCombination(int length);
int nextCombination(int *r, int n, int k);

#endif  // end of __MYZ_H__

