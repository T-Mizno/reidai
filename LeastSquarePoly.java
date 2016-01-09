import java.util.*;

public class LeastSquarePoly
{
    public int Order;
    public double[] cofOfTerms;

    public LeastSquarePoly(double[] x, double[] y, int _Order)
    {
        Order = _Order;
      
        double[][] A = new double[x.length][Order+1];

        for(int i=0; i<x.length; i++) {
            for(int j=0; j<=Order; j++) {
                A[i][j] = Math.pow(x[i], j);
            }
        }


        cofOfTerms = Myz.GaussLeastSquare(A, y);
    }

    public double value(double x)
    {
        double sum = 0.0;
        for(int i=0; i<=Order; i++) {
            sum += cofOfTerms[i] * Math.pow(x, i);
        }
        return sum;
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

        LeastSquarePoly lsp = new LeastSquarePoly(x, y, n);

        int N = 100;
        for(int i=0; i<=N; i++) {
            double testx = 10.0 /N * i - 5;
            System.out.println(testx + ", "+ lsp.value(testx));
        }

    }
}
