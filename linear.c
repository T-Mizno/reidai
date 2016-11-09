#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "myz.h"
#include "linear.h"

void _swap(int *a, int *b)
{
  int tmp;
  tmp = *a;
  *a = *b;
  *b = tmp;
  return;
}
void rowSelect(double **A, int *order, int pivot, int N)
{
  int i;
  double max;
  int maxNum;
  double tmp;

  maxNum = pivot;
  max = 0.0;
  ForEach(i, pivot, N-1)
    {
      tmp = fabs(A[order[i]][pivot]);
      if( tmp > max )
	{
	  max = tmp;
	  maxNum = i;
	}
    }
  _swap( &order[pivot],&order[maxNum]);
  return;
}
  

int Gauss(double **A, double *x, double *b, int N)
{
  int i, j, k;
  int *p;

  p = newVectorI(N);
  ForEach(i, 0, N-1)p[i] = i;

  // forward
  ForEach(k, 0, N-1)
    {
      rowSelect(A, p, k, N);
      ForEach(i,  k+1, N-1)
	{
	  A[p[i]][k] /= A[p[k]][k];
	
	  if(A[p[k]][k] == 0.0)return MATRIX_IS_SINGULAR;
	    ForEach(j, k+1, N-1)
	    {
	      A[p[i]][j] -= A[p[k]][j] * A[p[i]][k];
	    }
	  b[p[i]] -= b[p[k]] * A[p[i]][k];
	}
    }


  rForEach(i, N-1, 0)
    {
      ForEach(j, i+1, N-1)
	{
	  b[p[i]] -= A[p[i]][j] * x[j];
	}
      x[i] = b[p[i]]/A[p[i]][i];
    }
  free(p);

  return GAUSS_SUCCESS;
}
  
double **inversMatrix(double **Aori, int N)
{
  int i,j;
  double **A;
  double **T;
  double *x;
  double *b;

  T = newMatrixD(N, N);

  ForEach(i, 0, N-1)
    {
      A = createMatrixCloneD(Aori, N, N);
      x = newVectorD(N);
      b = newVectorD(N);
      ForEach(j, 0, N-1) b[j] = 0.0;
      b[i] = 1.0;
      Gauss(A, x, b, N);
      ForEach(j, 0, N-1)T[j][i] = x[j];
      freeMatrixD(A);
      free(x);
      free(b);
    }
  return T;
}
/*
int main()
{
  double **A;
  double b[3] = {1.0, -2.0, 7.0};
  double x[3];
  int tmp;

  A = readMatrixFromFile("matrix.dat", &tmp, 3);
  Gauss(A, x, b, 3);
  {
    int i,j;
    ForEach(i, 0, 2)
      {
	ForEach(j, 0, 2)
	  printf("%8.4f", A[i][j]);
	printf("\n");
      }
  }
  {
    int i;
    ForEach(i, 0, 2)
      {
	printf("%8.4f\n", x[i]);
      }
  }
  return 0;
}  
*/
