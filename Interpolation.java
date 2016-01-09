public class Interpolation
{
    public static double func(double x)
    {
        //return Math.exp(- x*x);
        return Math.sin(x);
    }
    public static void main(String[] argv)
    {
        int n = 5;

        double[] x = new double[n];
        double[] y = new double[n];

        for(int i=0; i<n; i++) {
            x[i] = 10.0/(n-1) * i - 5;
            y[i] = func(x[i]);
            System.out.println(x[i] + ", "+ y[i]);
        }

        LeastSquarePoly lsp = new LeastSquarePoly(x, y, n);
        Spline sp = new Spline(x, y, n);
        Spline sp1 = new Spline(x, y, n, 0.1, 0.1);

        System.out.println("x, poly, naturalsp, sp01");
        int N = 100;
        for(int i=0; i<=N; i++) {
            double testx = 14.0 /N * i - 7;
            System.out.println(testx + ", "+lsp.value(testx)+", "+ sp.f(testx)+", "+sp1.f(testx));
        }

    }
}
