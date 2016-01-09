import java.util.*;

public class Myz
{
    public static void multi(double[][] A, double[][] B, double[][] AB)
    {
        assert A[0].length == B.length;
        assert A.length == AB.length;
        assert B[0].length == AB[0].length;

        for(int i=0; i<A.length; i++) {
            for(int j=0; j<B[0].length; j++) {
                AB[i][j] = 0.0;
                for(int k=0; k<A[0].length; k++) {
                    AB[i][j] += A[i][k] * B[k][j];
                }
            }
        }
    }
    public static double[][] multi(double[][] A, double[][] B)
    {
        assert A[0].length == B.length;

        double[][] AB = new double[A.length][B[0].length];
        multi(A, B, AB);
        return AB;
    }

    public static void multi(double[][] A, double[] x, double[] Ax)
    {
        assert A[0].length == x.length;
        assert A.length == Ax.length;

        for(int i=0; i<A.length; i++){
            Ax[i] = 0.0;
            for(int j=0; j<A[0].length; j++){
                Ax[i] += A[i][j] * x[j];
            }
        }
    }
    public static double[] multi(double[][] A, double[] x)
    {
        assert A[0].length == x.length;

        double[] Ax = new double[A.length];
        multi(A, x, Ax);
        return Ax;
    }

    public static double[] add(double[] a, double[] b)
    {
        assert a.length == b.length;

        double[] c = new double[a.length];
        add(a, b, c);
        return c;
    }
    public static void add(double[] a, double[] b, double[] c)
    {
        assert a.length == b.length;
        assert a.length == c.length;

        for(int i=0; i<a.length; i++){
            c[i] = a[i] + b[i];
        }
    }

    public static double[][] trans(double[][] A)
    {
        double[][] tA = new double[A[0].length][A.length];

        for(int i=0; i<A.length; i++){
            for(int j=0; j<A[0].length; j++){
                tA[j][i] = A[i][j];
            }
        }
        return tA;
    }

    public static double[][] randomMatrix(int m, int n)
    {
        assert m > 0;
        assert n > 0;

        double[][] mat = new double[m][n];
        for(int i=0; i<m; i++){
            for(int j=0; j<n; j++){
                mat[i][j] = Math.random()*100;
            }
        }
        return mat;
    }

    public static double[] randomVector(int m)
    {
        assert m > 0;

        double[] vec = new double[m];
        for(int i=0; i<m; i++){
            vec[i] = Math.random()*100;
        }
        return vec;
    }
    
    public static double norm2(double[] a)
    {
        double x = 0.0;
        for(int i=0; i<a.length; i++){
            x += a[i] * a[i];
        }
        return Math.sqrt(x);
    }

    public static void stdout(double[][] A)
    {
        for(int i=0; i<A.length; i++){
            for(int j=0; j<A[0].length; j++){
                System.out.print("  "+A[i][j]);
            }
            System.out.println();
        }
    }
    public static void stdout(double[] x)
    {
        for(int j=0; j<x.length; j++){
            System.out.print("  "+x[j]);
        }
        System.out.println();
    }

    public static double diff(double[] a, double[] b)
    {
        assert a.length == b.length;

        double x = 0.0;
        for(int i=0; i<a.length; i++){
            x += Math.pow(a[i] - b[i], 2);
        }
        return Math.sqrt(x);
    }

    // use Gauss
    public static double[] GaussLeastSquare(double[][] A, double[] b)
    {
      double[][] At  = trans(A);
      double[][] AtA = multi(At, A);
      double[]   Atb = multi(At, b);

      Gauss g = new Gauss(AtA, Atb);
      g.gauss();

      return g.x;
    }

    public static void main(String[] argv)
    {
        double[][] A58 = {{1, 3, 3, 2}
                          ,{2, 6, 9, 5}
                          ,{-1, -3, 3, 0}};
        double[] b58 = {1, 2, -1};

        double[][] A20 = {{2, 1, 1}
                          ,{4,1,0}
                          ,{-2, 2,1}};
        double[] b20 = {1, -2, 7};

        double[][] A65 = {
            {0,0}
            , {1,2}
            , {4,8}
            , {0, 0}
        };
        double[] b65 = {0, 1, 4, 0};

        double[][] A132 = {{1,0}
                           ,{1,1}
                           ,{1,3}
                           ,{1,4}};
        double[] b132 = {0,1,2,5};
	
        int M = 30;
        int N = 30;
	
        double[][] A = randomMatrix(M, N);
        double[] xans = randomVector(N);
        double[] b = multi(A, xans);

        Gauss G = new Gauss(A, b);
        //System.out.println("ans x:");
        //stdout(xans);
        System.out.println("start");
        long start = System.currentTimeMillis();
        G.gauss();
        long end = System.currentTimeMillis();
        System.out.println("done");
        //G.stdout("done");

        System.out.println("time : " + (((double)end - (double)start)/1000.0)  + "s");
        System.out.print("diff of b:  ");
        double[] bNew = multi(A, G.x);
        for(int f=0; f<G.xs.length; f++) add(multi(A, G.xs[f]), bNew, bNew);
        System.out.println(diff(b, bNew)/norm2(b));

        stdout(GaussLeastSquare(A132, b132));
    }
}

class Gauss
{
    double[][] A;
    double[] b;
    int M;
    int N;
    int[] P;

    Vector<int[]> pivots;

    double[] x;
    double[][] xs;

    double epsillon = 1.0e-7;

    public Gauss(double[][] aA, double[] ab)
    {
        M = aA.length;
        N = aA[0].length;
        A = new double[M][N];
        b = new double[M];
        P = new int[M];
	
        pivots = new Vector<int[]>();
	
        x = new double[N];
        xs = new double[0][N];

        for(int i=0; i<M; i++) {
            for(int j=0; j<N; j++) {
                A[i][j] = aA[i][j];
            }
            b[i] = ab[i];
            P[i] = i;
        }	
    }

    void gauss()
    {
        forward();

        for(int j=0; j<N; j++) { x[j] = 0.0; }
        backward();

        {
            Vector<Integer> freeJs = new Vector<Integer>();
            double[] tmpB = new double[M];

            for(int j=0; j<N; j++) { if(isFreeJ(j)) freeJs.add(new Integer(j));}
            for(int i=0; i<M; i++) { tmpB[i] = 0.0; }
            xs = new double[freeJs.size()][N];
            for(int f=0; f<freeJs.size(); f++){
                for(int j=0; j<N; j++) {xs[f][j] = 0.0;}
                xs[f][freeJs.elementAt(f).intValue()] = 1.0;
                backward(tmpB, xs[f]);
            }
        }
    }

    void forward()
    {
        int pi=0, pj=0;
        for(pi=0; pi<M; pi++){
            pj = nextPivotj(pi, pj);
            if(! ijInRange(pi, pj)) return;
            updateP(pi, pj);
            addPivot(pi, pj);
            for(int i=pi+1; i<M; i++){
                forwardOneLine(pi, pj, i);
            }
            pj = pj + 1;
        }
    }
    void forwardOneLine(int pi, int pj, int i)
    {
        double pivot = A[P[i]][pj] / A[P[pi]][pj];
        A[P[i]][pj] = pivot;
        for(int j=pj + 1; j<N; j++){
            A[P[i]][j] = A[P[i]][j] - (pivot * A[P[pi]][j]);
        }
        b[P[i]] = b[P[i]] - (pivot * b[P[pi]]);
    }

    void updateP(int pi, int pj)
    {
        double max = Math.abs(A[P[pi]][pj]);
        int maxPI = pi;

        for(int i=pi+1; i<M; i++) {
            if(Math.abs(A[P[i]][pj]) > max) {
                max = Math.abs(A[P[i]][pj]);
                maxPI = i;
            }
        }
	
        //swap
        {
            int tmp;
            tmp = P[pi];
            P[pi] = P[maxPI];
            P[maxPI] = tmp;
        }
    }

    boolean isZero(double val)
    {
        return Math.abs(val) < epsillon;
    }

    boolean ijInRange(int row, int col)
    {
        return (row >= 0) && (col >= 0) && (row < M) && (col < N);
    }

    boolean underColumnAreZero(int pi, int pj)
    {
        for(int i=pi; i<M; i++){
            if(! isZero(A[P[i]][pj])) return false;
        }
        return true;
    }

    int nextPivotj(int pi, int pj)
    {
        for(int j=pj; j<N; j++){
            if(! underColumnAreZero(pi, j)) return j;
        }
        return -1; // fail
    }
    
    void addPivot(int pi, int pj)
    {
        int[] pair = new int[2];
        pair[0] = pi;
        pair[1] = pj;
        pivots.add(pair);
    }

    boolean isFreeI(int row)
    {
        for(int i=0; i<pivots.size(); i++){
            if(pivots.elementAt(i)[0] == row) return false;
        }
        return true;
    }
    boolean isFreeJ(int col)
    {
        for(int i=0; i<pivots.size(); i++){
            if(pivots.elementAt(i)[1] == col) return false;
        }
        return true;
    }

    void backward()
    {
        backward(b, x);
    }

    void backward(double[] ab, double[] ax)
    {
        for(int p=pivots.size()-1; p>=0; p--) {
            double sum = 0.0;
            int pi = pivots.elementAt(p)[0];
            int pj = pivots.elementAt(p)[1];
            for(int k=pj+1; k<N; k++) {
                sum = sum + A[P[pi]][k] * ax[k];
            }
            ax[pj] = (ab[P[pi]]-sum) / A[P[pi]][pj];
        }

    }

    public boolean isSolvable()
    {
        for(int i=0; i<M; i++) {
            if( isFreeI(i) && (! isZero(b[P[i]])) ) return false;
        }
        return true;
    }
    public void stdout()
    {
        for(int i=0; i<M; i++){
            System.out.print("("+P[i]+") ");
            for(int j=0; j<N; j++){
                System.out.print(" ");
                System.out.print(A[P[i]][j]);
            }
            System.out.print(" | ");
            System.out.println(b[P[i]]);
        }

        System.out.print("pivots : ");
        for(int i=0; i<pivots.size(); i++) {
            System.out.print(": (" + P[pivots.elementAt(i)[0]] + ", "+ pivots.elementAt(i)[1] + ")");
        }
        System.out.println();

        System.out.print("freeI :");
        for(int i=0; i<M; i++){
            if(isFreeI(i)) System.out.print(", "+P[i]);
        }
        System.out.println();
        System.out.print("freeJ :");
        for(int j=0; j<N; j++){
            if(isFreeJ(j)) System.out.print(", "+j);
        }
        System.out.println();

        System.out.println("Solvable: " + isSolvable());
        if(! isSolvable()) return;

        System.out.print("x : ");
        for(int j=0; j<x.length; j++){System.out.print("  "+ x[j]);}
        System.out.println();

        for(int f=0; f<xs.length; f++){
            System.out.print("x"+f+" : ");
            for(int j=0; j<xs[f].length; j++){System.out.print("  "+ xs[f][j]);}
            System.out.println();
        }
    }
    
    public void stdout(String msg)
    {
        System.out.println(msg);
        stdout();
    }

}
