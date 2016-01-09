import java.text.*;
import java.util.*;

public class Spline// implements Interpolation
{
    int N; // Size of given data set.

    // given data
    public double[] x;   // 0 - N-1
    public double[] f;   // 0 - N-1

    double[] h;   // 0 - N-2
    double[] f1;  // 0 - N-2
    double[] f2;  // 1 - N-2

    public Spline(double[] _x, double[] _f, int _N)
    {
        this(_x, _f, _N, 0.0, 0.0);
    }
    public Spline(double[] _x, double[] _f, int _N, double boundL, double boundR)
    {
        double[][] _tmpA = new double[_N][2];
        for(int i=0; i<_N; i++)
            {
                _tmpA[i][0] = _x[i];
                _tmpA[i][1] = _f[i];
            }

        double[] _tmpX = new double[_N];
        double[] _tmpF = new double[_N];
        for(int i=0; i<_N; i++)
            {
                _tmpX[i] = _tmpA[i][0];
                _tmpF[i] = _tmpA[i][1];
            }
        setDataSet(_tmpX, _tmpF, _N, boundL, boundR);
    }

    public boolean setDataSet(double[] _x, double[] _f, int _N,
                              double boundL, double boundR)
    {
        N = _N;

        x = new double[N];
        f = new double[N];
        h = new double[N-1];
        f1 = new double[N-1];
        f2 = new double[N];

        for(int i=0; i<N; i++)
            {
                x[i] = _x[i];
                f[i] = _f[i];
            }

        for(int i=0; i<N-1; i++) h[i] = x[i+1] - x[i];

      
        _set_y2(boundL, boundR);
        _set_y1();

        return true;
    }

    boolean _set_y2(double boundL, double boundR)
    {
        double[][] H = new double[N-2][N-2];
        double[] y = new double[N-2];
        double[] b = new double[N-2];

        for(int i=0; i<N-2; i++)
            H[i][i] = 2 * ( h[i] + h[i+1]);
        for(int i=0; i<N-3; i++)
            H[i][i+1] = h[i+1];
        for(int i=1; i<N-2; i++)
            H[i][i-1] = h[i-1];

        for(int i=0; i<N-2; i++)
            b[i] = 
                6.0 * ( (f[i+2] - f[i+1])/h[i+1] - (f[i+1] - f[i])/h[i] );

        Gauss g = new Gauss(H, b);
        g.gauss();
        y = g.x;
      
        f2[0] = boundL;
        for(int i=1; i<N-1; i++) f2[i] = y[i-1];
        f2[N-1] = boundR;

        return true;
    }
  
    boolean _set_y1()
    {
        for(int i=0; i<N-1; i++)
            {
                f1[i] = (f[i+1] - f[i])/ h[i] - f2[i+1] * h[i] / 6.0 - f2[i] * h[i] / 3.0;
            }

        return true;
    }
  
    int _searchInterval(double _x)
    {
        if(_x < x[0]) return 0;

        for(int i=0; i<N-1; i++)
            {
                if( (x[i] <= _x) && (_x <= x[i+1]) ) return i;
            }
        return N-2;
    }

    double splinePoly(double _x, int i)
    {
        return
            f[i]
            + f1[i] * (_x - x[i])
            + f2[i] * Math.pow( (_x - x[i]), 2.0 )/2.0
            + (f2[i+1] - f2[i]) 
            * Math.pow( (_x - x[i]), 3.0)/(6.0 * h[i]);
    }
  
    double splinePolyDev1(double _x, int i)
    {
        return
            f1[i]
            + f2[i] * (_x - x[i])
            + (f2[i+1] - f2[i])
            * Math.pow( (_x - x[i]), 2.0)/(2.0 * h[i]);
    }

    public double f(double _x)
    {
        int i = _searchInterval(_x);
        return splinePoly(_x, i);
    }
    public double dev1(double _x)
    {
        int i = _searchInterval(_x);
        return splinePolyDev1(_x, i);
    }

    public double x(int i) { return x[i]; }
    public double f(int i) { return f[i]; }
    public int N() { return N; }

    public void stdout()
    {
        DecimalFormat form = new DecimalFormat("######.####");

        String str = new String();
        str += "x  h f f1 f2\n";
        for(int i=0; i<N; i++)
            {
                str += form.format(x[i]);

                if(i <= N-2)
                    str += " " + form.format(h[i]);
                else
                    str += " -----";

                str += " " + form.format(f[i]);

                if(i <= N-2)
                    str += " " + form.format(f1[i]);
                else
                    str += " -----";

                str += " " + form.format(f2[i]);

                str += "\n";
            }
        System.out.println(str);
        return;
    }
  
    public static void main(String[] argv)
    {
        int n = 11;

        double[] x = new double[n];
        double[] y = new double[n];

        for(int i=0; i<n; i++) {
            x[i] = 10.0/(n-1) * i - 5;
            y[i] = Math.exp(- x[i]*x[i]);
        }

        Spline lsp = new Spline(x, y, n);
        //Spline lsp = new Spline(x, y, n, 0.5, 0.5);

        int N = 100;
        for(int i=0; i<=N; i++) {
            double testx = 14.0 /N * i - 7;
            System.out.println(testx + ", "+ lsp.f(testx));
        }

    }
}
