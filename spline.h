#ifndef __SPLINE_H__
#define __SPLINE_H__

struct _dataForSpline 
{ 
  int N;
  double *x;   // 0 - N-1
  double *y;   // 0 - N-1
  double *h;   // 0 - N-2
  double *y1;  // 0 - N-2
  double *y2;  // 1 - N-2
};
typedef struct _dataForSpline Spline;


Spline *newSpline(double *x, double *y, int N);
Spline *newSplineWithBound(double *x, double *y, int N, double b1, double b2);
double splineValue(Spline *data, double x);
double splineValueDev1(Spline *data, double x);
void freeSpline(Spline *S);

void stdoutSpline(Spline *data);


#endif // __SPLINE_H__
