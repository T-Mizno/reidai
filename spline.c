#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#include "myz.h"
#include "linear.h"

#include "spline.h"

int _set_y2(Spline *data, double bound1, double bound2)
{
  double **H;
  double *y;
  double *b;
  int i, j;

  H = newMatrixD(data->N-2, data->N-2);
  y = newVectorD(data->N-2);
  b = newVectorD(data->N-2);

  /*
  for(i=0; i<data->N-2; i++)
    H[i][i] = 2 * ( data->h[i] + data->h[i+1]);
  for(i=0; i<data->N-3; i++)
    H[i][i+1] = data->h[i+1];
  for(i=1; i<data->N-2; i++)
    H[i][i-1] = data->h[i-1];


  for(i=0; i<data->N-2; i++)
    b[i] = 
      6.0 * ( (data->y[i+2] - data->y[i+1])/data->h[i+1]
	      - (data->y[i+1] - data->y[i])/data->h[i]
	      );
	      */

  for(i=0; i<data->N-2; i++)
    {
      for(j=0; j<data->N-2; j++)
	H[i][j] = 0.0;
    }

  for(i=0; i<data->N-2; i++)
    {
      if(i > 0) H[i][i-1] = data->h[i];
      H[i][i] = 2 * ( data->h[i + 1] + data->h[i+2]);
      if(i < data->N-3) H[i][i+1] = data->h[i+3];
    }

  for(i=0; i<data->N-2; i++)
    b[i] = 
      6.0 * ( (data->y[i+2] - data->y[i+1])/data->h[i+1]
	      - (data->y[i+1] - data->y[i])/data->h[i]
	      );
  b[0] -= bound1 * data->h[0];
  b[data->N-3] -= bound2 * data->h[data->N-3];

  /*
  for(i=0; i<data->N-2; i++)
    {
      for(j=0; j<data->N-2; j++)
	printf("%5.2f", H[i][j]);
      printf("\n");
    }
      printf("\n");
      printf("\n");
      */
  
  /*
  {
    int j;
    for(i=0; i<data->N-2; i++)
      {
	for(j=0; j<data->N-2; j++)
	  printf("%7.3f ", H[i][j]);
	printf(" | %7.3f\n", b[i]);
      }
  }
  */

  Gauss(H, y, b, data->N-2);
  
  data->y2 = newVectorD(data->N);
  data->y2[0] = bound1;
  for(i=1; i<data->N-1; i++)
    data->y2[i] = y[i-1];
  data->y2[data->N-1] = bound2;

  freeMatrixD(H);
  free(y);
  free(b);
  return 0;
}

int _set_y1(Spline *data)
{
  int i;

  data->y1 = newVectorD(data->N-1);
  for(i=0; i<data->N-1; i++)
    {
      data->y1[i] =
	(data->y[i+1] - data->y[i])/ data->h[i]
	- data->y2[i+1] * data->h[i] / 6.0
	- data->y2[i] * data->h[i] / 3.0;
    }
  return 0;
}

Spline *newSplineWithBound(double *x, double *y, int N, double b1, double b2)
{
  Spline *newData;
  int i;

  newData = (Spline *)malloc(sizeof(Spline));

  // set x and y
  newData->x = newVectorD(N);
  newData->y = newVectorD(N);
  newData->N = N;
  for(i=0; i<N; i++)
    {
      newData->x[i] = x[i];
      newData->y[i] = y[i];
    }

  // set h
  newData->h = newVectorD(N-1);
  for(i=0; i<N-1; i++)
    {
      newData->h[i] = x[i+1] - x[i];
    }
  _set_y2(newData, b1, b2);
  _set_y1(newData);

  return newData;
}
Spline *newSpline(double *x, double *y, int N)
{
  return newSplineWithBound(x, y, N, 0.0, 0.0);
}

int _searchInterval(Spline *data, double x)
{
  int i;

  //if(x < data->x[0]) return -2;
  //if(x > data->x[data->N-1]) return -1;
  if(x < data->x[0]) return 0;

  for(i=0; i<data->N-1; i++)
    if( (data->x[i] <= x) && (x <= data->x[i+1]) )
      return i;
return data->N-2;
}

double splinePoly(Spline *data, double x, int i)
{
  return
    data->y[i]
    + data->y1[i] * (x - data->x[i])
    + data->y2[i] * pow( (x - data->x[i]), 2.0 )/2.0
    + (data->y2[i+1] - data->y2[i]) 
       * pow( (x - data->x[i]), 3.0)/(6.0 * data->h[i]);
}
double splinePolyDev1(Spline *data, double x, int i)
{
  return
    data->y1[i]
    + data->y2[i] * (x - data->x[i])
    + (data->y2[i+1] - data->y2[i])
       * pow( (x - data->x[i]), 2.0)/(2.0 * data->h[i]);
}
double splineValue(Spline *data, double x)
{
  int i;

  i = _searchInterval(data, x);
  return splinePoly(data, x, i);
}
double splineValueDev1(Spline *data, double x)
{
  int i;

  i = _searchInterval(data, x);
  return splinePolyDev1(data, x, i);
}
  
void stdoutSpline(Spline *data)
{
  int i;
  printf("%7s %7s %7s %7s %7s\n", "x", "h", "y", "y1", "y2");
  for(i=0; i<data->N; i++)
    {
      printf("%7.3f",data->x[i]);

      if(i <= data->N-2)
	printf(" %7.3f", data->h[i]);
      else
	printf(" %7s", "-----");

      printf(" %7.3f", data->y[i]);

      if(i <= data->N-2)
	printf(" %7.3f", data->y1[i]);
      else
	printf(" %7s", "-----");

      printf(" %7.3f", data->y2[i]);

      printf("\n");
    }
  return;
}

void freeSpline(Spline *S)
{
  free(S->x);
  free(S->y);
  free(S->h);
  free(S->y1);
  free(S->y2);
  free(S);
  return;
}

/*
int main(void)
{
  double **tmp;
  double *x, *y;
  int N;

  Spline *data;
  double test;
  int i;

  FILE *fout;

  N = readMatrixFromFileD("data.dat", &tmp, 2);

  x = newVectorD(N);
  y = newVectorD(N);
 
  for(i=0; i<N; i++)
    {
      x[i] = tmp[i][0];
      y[i] = tmp[i][1];
    }

  data = newSpline(x, y, N);
  fout = fopen("log", "w");
  for(i=0; i<100; i++)
    {
      test = -0.3 + (1.4 - 0.1)/100 * i;
      fprintf(fout, "%7.3f %7.3f\n", test, splineValueDev1(data, test));
    }
  fclose(fout);
  //stdoutSpline(data);

  free(x);
  free(y);
  freeMatrixD(tmp);
  return 0;
}

*/
