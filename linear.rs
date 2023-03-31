use std::ops::{Deref, DerefMut};

fn is_zero(v: NumType) -> bool {
    v.abs() < 1e-10
}

type NumType = f64;

// 乱数列の代わり pi
static ARRAY_PI: [NumType; 289] = [
    0.3, 0.1, 0.4, 0.1, 0.5, 0.9, 0.2, 0.6, 0.5, 0.3, 0.5, 0.8, 0.9, 0.7, 0.9, 0.3, 0.2, 0.3, 0.8,
    0.4, 0.6, 0.2, 0.6, 0.4, 0.3, 0.3, 0.8, 0.3, 0.2, 0.7, 0.9, 0.5, 0.0, 0.2, 0.8, 0.8, 0.4, 0.1,
    0.9, 0.7, 0.1, 0.6, 0.9, 0.3, 0.9, 0.9, 0.3, 0.7, 0.5, 0.1, 0.0, 0.5, 0.8, 0.2, 0.0, 0.9, 0.7,
    0.4, 0.9, 0.4, 0.4, 0.5, 0.9, 0.2, 0.3, 0.0, 0.7, 0.8, 0.1, 0.6, 0.4, 0.0, 0.6, 0.2, 0.8, 0.6,
    0.2, 0.0, 0.8, 0.9, 0.9, 0.8, 0.6, 0.2, 0.8, 0.0, 0.3, 0.4, 0.8, 0.2, 0.5, 0.3, 0.4, 0.2, 0.1,
    0.1, 0.7, 0.0, 0.6, 0.7, 0.9, 0.8, 0.2, 0.1, 0.4, 0.8, 0.0, 0.8, 0.6, 0.5, 0.1, 0.3, 0.2, 0.8,
    0.2, 0.3, 0.0, 0.6, 0.6, 0.4, 0.7, 0.0, 0.9, 0.3, 0.8, 0.4, 0.4, 0.6, 0.0, 0.9, 0.5, 0.5, 0.0,
    0.5, 0.8, 0.2, 0.2, 0.3, 0.1, 0.7, 0.2, 0.5, 0.3, 0.5, 0.9, 0.4, 0.0, 0.8, 0.1, 0.2, 0.8, 0.4,
    0.8, 0.1, 0.1, 0.1, 0.7, 0.4, 0.5, 0.0, 0.2, 0.8, 0.4, 0.1, 0.0, 0.2, 0.7, 0.0, 0.1, 0.9, 0.3,
    0.8, 0.5, 0.2, 0.1, 0.1, 0.0, 0.5, 0.5, 0.5, 0.9, 0.6, 0.4, 0.4, 0.6, 0.2, 0.2, 0.9, 0.4, 0.8,
    0.9, 0.5, 0.4, 0.9, 0.3, 0.0, 0.3, 0.8, 0.1, 0.9, 0.6, 0.4, 0.4, 0.2, 0.8, 0.8, 0.1, 0.0, 0.9,
    0.7, 0.5, 0.6, 0.6, 0.5, 0.9, 0.3, 0.3, 0.4, 0.4, 0.6, 0.1, 0.2, 0.8, 0.4, 0.7, 0.5, 0.6, 0.4,
    0.8, 0.2, 0.3, 0.3, 0.7, 0.8, 0.6, 0.7, 0.8, 0.3, 0.1, 0.6, 0.5, 0.2, 0.7, 0.1, 0.2, 0.0, 0.1,
    0.9, 0.0, 0.9, 0.1, 0.4, 0.5, 0.6, 0.4, 0.8, 0.5, 0.6, 0.6, 0.9, 0.2, 0.3, 0.4, 0.6, 0.0, 0.3,
    0.4, 0.8, 0.6, 0.1, 0.0, 0.4, 0.5, 0.4, 0.3, 0.2, 0.6, 0.6, 0.4, 0.8, 0.2, 0.1, 0.3, 0.3, 0.9,
    0.3, 0.6, 0.0, 0.7,
];

// 乱数列の代わり e
static ARRAY_E: [NumType; 426] = [
    0.2, 0.7, 0.1, 0.8, 0.2, 0.8, 0.1, 0.8, 0.2, 0.8, 0.4, 0.5, 0.9, 0.0, 0.4, 0.5, 0.2, 0.3, 0.5,
    0.3, 0.6, 0.0, 0.2, 0.8, 0.7, 0.4, 0.7, 0.1, 0.3, 0.5, 0.2, 0.6, 0.6, 0.2, 0.4, 0.9, 0.7, 0.7,
    0.5, 0.7, 0.2, 0.4, 0.7, 0.0, 0.9, 0.3, 0.6, 0.9, 0.9, 0.9, 0.5, 0.9, 0.5, 0.7, 0.4, 0.9, 0.6,
    0.6, 0.9, 0.6, 0.7, 0.6, 0.2, 0.7, 0.7, 0.2, 0.4, 0.0, 0.7, 0.6, 0.6, 0.3, 0.0, 0.3, 0.5, 0.3,
    0.5, 0.4, 0.7, 0.5, 0.9, 0.4, 0.5, 0.7, 0.1, 0.3, 0.8, 0.2, 0.1, 0.7, 0.8, 0.5, 0.2, 0.5, 0.1,
    0.6, 0.6, 0.4, 0.2, 0.7, 0.4, 0.2, 0.7, 0.4, 0.6, 0.6, 0.3, 0.9, 0.1, 0.9, 0.3, 0.2, 0.0, 0.0,
    0.3, 0.0, 0.5, 0.9, 0.9, 0.2, 0.1, 0.8, 0.1, 0.7, 0.4, 0.1, 0.3, 0.5, 0.9, 0.6, 0.6, 0.2, 0.9,
    0.0, 0.4, 0.3, 0.5, 0.7, 0.2, 0.9, 0.0, 0.0, 0.3, 0.3, 0.4, 0.2, 0.9, 0.5, 0.2, 0.6, 0.0, 0.5,
    0.9, 0.5, 0.6, 0.3, 0.0, 0.7, 0.3, 0.8, 0.1, 0.3, 0.2, 0.3, 0.2, 0.8, 0.6, 0.2, 0.7, 0.9, 0.4,
    0.3, 0.4, 0.9, 0.0, 0.7, 0.6, 0.3, 0.2, 0.3, 0.3, 0.8, 0.2, 0.9, 0.8, 0.8, 0.0, 0.7, 0.5, 0.3,
    0.1, 0.9, 0.5, 0.2, 0.5, 0.1, 0.0, 0.1, 0.9, 0.0, 0.1, 0.1, 0.5, 0.7, 0.3, 0.8, 0.3, 0.4, 0.1,
    0.8, 0.7, 0.9, 0.3, 0.0, 0.7, 0.0, 0.2, 0.1, 0.5, 0.4, 0.0, 0.8, 0.9, 0.1, 0.4, 0.9, 0.9, 0.3,
    0.4, 0.8, 0.8, 0.4, 0.1, 0.6, 0.7, 0.5, 0.0, 0.9, 0.2, 0.4, 0.4, 0.7, 0.6, 0.1, 0.4, 0.6, 0.0,
    0.6, 0.6, 0.8, 0.0, 0.8, 0.2, 0.2, 0.6, 0.4, 0.8, 0.0, 0.0, 0.1, 0.6, 0.8, 0.4, 0.7, 0.7, 0.4,
    0.1, 0.1, 0.8, 0.5, 0.3, 0.7, 0.4, 0.2, 0.3, 0.4, 0.5, 0.4, 0.4, 0.2, 0.4, 0.3, 0.7, 0.1, 0.0,
    0.7, 0.5, 0.3, 0.9, 0.0, 0.7, 0.7, 0.7, 0.4, 0.4, 0.9, 0.9, 0.2, 0.0, 0.6, 0.9, 0.5, 0.5, 0.1,
    0.7, 0.0, 0.2, 0.7, 0.6, 0.1, 0.8, 0.3, 0.8, 0.6, 0.0, 0.6, 0.2, 0.6, 0.1, 0.3, 0.3, 0.1, 0.3,
    0.8, 0.4, 0.5, 0.8, 0.3, 0.0, 0.0, 0.0, 0.7, 0.5, 0.2, 0.0, 0.4, 0.4, 0.9, 0.3, 0.3, 0.8, 0.2,
    0.6, 0.5, 0.6, 0.0, 0.2, 0.9, 0.7, 0.6, 0.0, 0.6, 0.7, 0.3, 0.7, 0.1, 0.1, 0.3, 0.2, 0.0, 0.0,
    0.7, 0.0, 0.9, 0.3, 0.2, 0.8, 0.7, 0.0, 0.9, 0.1, 0.2, 0.7, 0.4, 0.4, 0.3, 0.7, 0.4, 0.7, 0.0,
    0.4, 0.7, 0.2, 0.3, 0.0, 0.6, 0.9, 0.6, 0.9, 0.7, 0.7, 0.2, 0.0, 0.9, 0.3, 0.1, 0.0, 0.1, 0.4,
    0.1, 0.6, 0.9, 0.2, 0.8, 0.3, 0.6, 0.8, 0.1, 0.9, 0.0, 0.2, 0.5, 0.5, 0.1, 0.5, 0.1, 0.0, 0.8,
    0.6, 0.5, 0.7, 0.4, 0.6, 0.3, 0.7, 0.7,
];

struct Matrix {
    _data: Vec<Vec<NumType>>,
    m: usize,
    n: usize,
}

// a[i][j] とアクセスできるように
impl Deref for Matrix {
    type Target = Vec<Vec<NumType>>;
    fn deref(&self) -> &Vec<Vec<NumType>> {
        &self._data
    }
}

// a[i][j] = val と代入できるように
impl DerefMut for Matrix {
    fn deref_mut(&mut self) -> &mut Vec<Vec<NumType>> {
        &mut self._data
    }
}

impl Matrix {
    fn new(a_m: usize, a_n: usize) -> Matrix {
        assert!(a_m >= 1 && a_n >= 1);

        let mut d: Vec<Vec<NumType>> = Vec::new();
        for _i in 0..a_m {
            let mut col: Vec<NumType> = Vec::new();
            for _j in 0..a_n {
                col.push(0.0);
            }
            d.push(col);
        }
        Matrix {
            _data: d,
            m: a_m,
            n: a_n,
        }
    }

    fn new_random(a_m: usize, a_n: usize, seed: usize) -> Matrix {
        assert!(a_m >= 1 && a_n >= 1);

        if seed % 2 == 0 {
            Matrix::new_from_array(a_m, a_n, &ARRAY_PI, seed)
        } else {
            Matrix::new_from_array(a_m, a_n, &ARRAY_E, seed)
        }
    }

    fn new_from_array(a_m: usize, a_n: usize, a: &[NumType], offset: usize) -> Matrix {
        assert!(a_m >= 1 && a_n >= 1);
        assert!(a.len() >= 1);

        let mut mat = Matrix::new(a_m, a_n);
        let mut i = 0;
        let mut j = 0;
        let mut k = offset % a.len();

        loop {
            if i >= mat.m {
                break;
            }
            if k >= a.len() {
                break;
            }

            mat[i][j] = a[k];

            k = (k + 1) % a.len();
            j = j + 1;
            if j >= mat.n {
                i = i + 1;
                j = 0;
            }
        }
        mat
    }

    fn copy(&self) -> Matrix {
        let mut new_mat = Matrix::new(self.m, self.n);
        new_mat.set(&self);
        new_mat
    }

    fn transpose(&self) -> Matrix {
        let mut new_mat = Matrix::new(self.n, self.m);
        for i in 0..self.m {
            for j in 0..self.n {
                new_mat[j][i] = self[i][j];
            }
        }
        new_mat
    }

    fn set(&mut self, from: &Matrix) {
        assert!(self.m <= from.m);
        assert!(self.n <= from.n);

        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] = from[i][j];
            }
        }
    }

    fn set_val(&mut self, val: NumType) {
        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] = val;
            }
        }
    }

    fn set_multi_val(&mut self, val: NumType) {
        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] = val * self[i][j];
            }
        }
    }

    fn accume_add(&mut self, a: &Matrix) {
        assert_eq!(self.m, a.m);
        assert_eq!(self.n, a.n);

        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] += a[i][j];
            }
        }
    }

    fn set_add(&mut self, a: &Matrix, b: &Matrix) {
        assert_eq!(a.m, b.m);
        assert_eq!(a.n, b.n);
        assert_eq!(self.m, a.m);
        assert_eq!(self.n, a.n);

        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] = a[i][j] + b[i][j];
            }
        }
    }

    fn set_sub(&mut self, a: &Matrix, b: &Matrix) {
        assert_eq!(a.m, b.m);
        assert_eq!(a.n, b.n);
        assert_eq!(self.m, a.m);
        assert_eq!(self.n, a.n);

        for i in 0..self.m {
            for j in 0..self.n {
                self[i][j] = a[i][j] - b[i][j];
            }
        }
    }

    fn set_multi(&mut self, a: &Matrix, b: &Matrix) {
        assert_eq!(self.m, a.m);
        assert_eq!(self.n, b.n);
        assert_eq!(a.n, b.m);

        self.set_val(0.0);

        for i in 0..a.m {
            for j in 0..b.n {
                for k in 0..a.n {
                    self[i][j] += a[i][k] * b[k][j];
                }
            }
        }
    }

    fn diff(a: &Matrix, b: &Matrix) -> NumType {
        assert_eq!(a.m, b.m);
        assert_eq!(a.n, b.n);

        let mut sum = 0.0;
        for i in 0..a.m {
            for j in 0..a.n {
                sum = sum + (a[i][j] - b[i][j]).abs();
            }
        }
        sum
    }

    fn are_same(a: &Matrix, b: &Matrix) -> bool {
        (a.m == b.m) && (a.n == b.n) && is_zero(Matrix::diff(&a, &b))
    }

    fn row_ids(&self) -> Vec<usize> {
        let mut p: Vec<usize> = Vec::new();
        for i in 0..self.m {
            p.push(i as usize);
        }
        p
    }

    fn stdout(&self) {
        for i in 0..self.m {
            for j in 0..self.n {
                print!(" {:8.4}", self[i][j]);
            }
            println!();
        }
    }
}

fn gauss_search_max_i_and_swap(p: &mut Vec<usize>, a: &Matrix, pivot_i: usize, pivot_j: usize) {
    assert_eq!(a.m, p.len());

    let mut max_i = pivot_i;
    let mut max_val: NumType = a[p[pivot_i]][pivot_j].abs();

    for i in (pivot_i + 1)..a.m {
        let val = a[p[i]][pivot_j].abs();
        if val > max_val {
            max_i = i;
            max_val = val;
        }
    }

    let tmp_i = p[pivot_i];
    p[pivot_i] = p[max_i];
    p[max_i] = tmp_i;
}

fn gauss_backwawrd(
    p: &Vec<usize>,
    a: &Matrix,
    b: &Matrix,
    rank: usize,
    base_js: &Vec<usize>,
    x: &mut Matrix,
) {
    for i in (0..rank).rev() {
        let mut sum = 0.0;
        for j in (base_js[i] + 1)..a.n {
            sum += a[p[i]][j] * x[j][0];
        }
        x[base_js[i]][0] = (b[p[i]][0] - sum) / a[p[i]][base_js[i]];
    }
}

fn gauss_p_stdout(p: &Vec<usize>, a: &Matrix) {
    assert_eq!(a.m, p.len());

    for i in 0..a.m {
        print!(" {:3}", p[i]);
        for j in 0..a.n {
            print!(" {:8.4}", a[p[i]][j]);
        }
        println!();
    }
}

struct GaussResult {
    x: Matrix,
    xs: Vec<Matrix>,
    rank: usize,
    base_js: Vec<usize>,
    p: Vec<usize>,
    is_solvable: bool,
}

fn gauss(a: &mut Matrix, b: &mut Matrix) -> GaussResult {
    let mut p = a.row_ids();

    let mut pivot_i = 0;
    let mut pivot_j = 0;

    let mut flg_base_js: Vec<bool> = Vec::new();
    for _j in 0..a.n {
        flg_base_js.push(false);
    }
    let mut base_js: Vec<usize> = Vec::new();

    let mut rank = 0;

    loop {
        if pivot_i > a.m - 1 {
            break;
        }

        loop {
            if pivot_j > a.n - 1 {
                break;
            }
            gauss_search_max_i_and_swap(&mut p, &a, pivot_i, pivot_j);
            if !is_zero(a[p[pivot_i]][pivot_j]) {
                break;
            }
            pivot_j = pivot_j + 1;
        }

        if pivot_j > a.n - 1 {
            break;
        }

        flg_base_js[pivot_j] = true;

        let pivot_val = a[p[pivot_i]][pivot_j];
        for i in (pivot_i + 1)..a.m {
            let rate = a[p[i]][pivot_j] / pivot_val;
            a[p[i]][pivot_j] = rate;
            for j in (pivot_j + 1)..a.n {
                a[p[i]][j] -= a[p[pivot_i]][j] * rate;
            }
            b[p[i]][0] -= b[p[pivot_i]][0] * rate;
        }

        pivot_i = pivot_i + 1;
        pivot_j = pivot_j + 1;
    }

    for j in 0..a.n {
        if flg_base_js[j] {
            rank += 1;
            base_js.push(j);
        }
    }

    let mut is_solvable = true;
    for i in (rank + 1)..a.m {
        is_solvable = is_solvable && is_zero(b[i][0]);
    }

    let mut x = Matrix::new(a.n, 1);
    x.set_val(0.0);
    gauss_backwawrd(&p, &a, &b, rank, &base_js, &mut x);

    let mut xs: Vec<Matrix> = Vec::new();
    let mut zero_b = b.copy();
    zero_b.set_val(0.0);

    for j in 0..a.n {
        if !flg_base_js[j] {
            let mut xg = x.copy();
            xg.set_val(0.0);
            xg[j][0] = 1.0;
            gauss_backwawrd(&p, &a, &zero_b, rank, &base_js, &mut xg);
            xs.push(xg);
        }
    }

    GaussResult {
        x: x,
        xs: xs,
        rank: rank,
        base_js: base_js,
        p: p,
        is_solvable: is_solvable,
    }
}

fn gauss_least_square(a: &Matrix, b: &Matrix) -> GaussResult {
    let a_t = a.transpose();
    let mut ata = Matrix::new(a.n, a.n);
    let mut atb = Matrix::new(a.n, 1);
    ata.set_multi(&a_t, &a);
    atb.set_multi(&a_t, &b);

    gauss(&mut ata, &mut atb)
}

fn main() {
    /*
    //p.10
    let mut a = Matrix::new_from_array(
        3,
        3,
        &[2.0, 1.0, 1.0, 4.0, 1.0, 0.0, -2.0, 2.0, 1.0, -3.0, 3.0, 0.0],
        0,
    );
    let mut b: Matrix = Matrix::new_from_array(3, 1, &[1.0, -2.0, 7.0], 0);
    */

    //p.58
    let mut a = Matrix::new_from_array(
        3,
        4,
        &[1.0, 3.0, 3.0, 2.0, 2.0, 6.0, 9.0, 5.0, -1.0, -3.0, 3.0, 0.0],
        0,
    );
    let mut b: Matrix = Matrix::new_from_array(3, 1, &[1.0, 2.0, -1.0], 0);

    /*
    //p.132
    let mut a = Matrix::new_from_array(4, 2, &[1.0, 0.0, 1.0, 1.0, 1.0, 3.0, 1.0, 4.0], 0);
    let mut b: Matrix = Matrix::new_from_array(4, 1, &[0.0, 1.0, 2.0, 5.0], 0);
    */

    println!("Before A");
    a.stdout();

    println!("Before b");
    b.stdout();

    let result = gauss(&mut a, &mut b);
    //let result = gauss_least_square(&mut a, &mut b);

    println!("After A");
    gauss_p_stdout(&result.p, &a);

    println!("After b");
    gauss_p_stdout(&result.p, &b);

    println!("Rank : {}", result.rank);
    println!("base var columns : {:?}", result.base_js);
    println!("x(Special)");
    result.x.stdout();

    for x in result.xs {
        println!("x(General)");
        x.stdout();
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
/// TEST
//////////////////////////////////////////////////////////////////////////////////////////

#[test]
fn test_add_sub() {
    let m = 55;
    let n = 13;

    let a = Matrix::new_random(m, n, 0);
    let mut b = Matrix::new_random(m, n, 101);

    let mut result_sub = a.copy();
    result_sub.set_sub(&a, &b);

    let mut result_add = a.copy();
    b.set_multi_val(-1.0);
    result_add.set_add(&a, &b);

    assert!(Matrix::are_same(&result_add, &result_sub));
}

#[test]
fn test_multi() {
    let a = Matrix::new_from_array(3, 2, &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 0);
    let b = Matrix::new_from_array(2, 3, &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 0);
    let ans = Matrix::new_from_array(
        a.m,
        b.n,
        &[9.0, 12.0, 15.0, 19.0, 26.0, 33.0, 29.0, 40.0, 51.0],
        0,
    );
    let mut c = Matrix::new(a.m, b.n);
    c.set_multi(&a, &b);

    assert!(Matrix::are_same(&c, &ans));
}

#[test]
fn test_multi2() {
    let m = 103;
    let k = 79;
    let n = 53;
    let a = Matrix::new_random(m, k, 30);
    let b = Matrix::new_random(k, n, 31);
    let mut ab = Matrix::new(a.m, b.n);
    ab.set_multi(&a, &b);
    let ab_t = ab.transpose();

    let a_t = a.transpose();
    let b_t = b.transpose();
    let mut bt_at = Matrix::new(b_t.m, a_t.n);
    bt_at.set_multi(&b_t, &a_t);

    assert!(Matrix::are_same(&ab_t, &bt_at));
}

#[test]
fn test_gauss_text_p10() {
    //p.10
    let mut a10 = Matrix::new_from_array(3, 3, &[2.0, 1.0, 1.0, 4.0, 1.0, 0.0, -2.0, 2.0, 1.0], 0);
    let mut b10: Matrix = Matrix::new_from_array(3, 1, &[1.0, -2.0, 7.0], 0);

    let x_ans = Matrix::new_from_array(3, 1, &[-1.0, 2.0, 1.0], 0);

    let r = gauss(&mut a10, &mut b10);

    assert!(r.is_solvable);
    assert_eq!(r.rank, 3);
    assert!(Matrix::are_same(&r.x, &x_ans));
    assert_eq!(r.xs.len(), 0);
}

#[test]
fn test_gauss_text_p58() {
    //p.58
    let mut a58 = Matrix::new_from_array(
        3,
        4,
        &[1.0, 3.0, 3.0, 2.0, 2.0, 6.0, 9.0, 5.0, -1.0, -3.0, 3.0, 0.0],
        0,
    );
    let mut b58: Matrix = Matrix::new_from_array(3, 1, &[1.0, 2.0, -1.0], 0);

    let x_ans = Matrix::new_from_array(4, 1, &[1.0, 0.0, 0.0, 0.0], 0);
    let xs_ans0 = Matrix::new_from_array(4, 1, &[-3.0, 1.0, 0.0, 0.0], 0);
    let xs_ans1 = Matrix::new_from_array(4, 1, &[-1.0, 0.0, -1.0 / 3.0, 1.0], 0);

    let r = gauss(&mut a58, &mut b58);

    assert!(r.is_solvable);
    assert_eq!(r.rank, 2);
    assert!(Matrix::are_same(&r.x, &x_ans));
    assert_eq!(r.xs.len(), 2);
    assert!(Matrix::are_same(&r.xs[0], &xs_ans0));
    assert!(Matrix::are_same(&r.xs[1], &xs_ans1));
}

#[test]
fn test_gauss_least_square_text_p132() {
    //p.132
    let a132 = Matrix::new_from_array(4, 2, &[1.0, 0.0, 1.0, 1.0, 1.0, 3.0, 1.0, 4.0], 0);
    let b132: Matrix = Matrix::new_from_array(4, 1, &[0.0, 1.0, 2.0, 5.0], 0);

    let x_ans = Matrix::new_from_array(2, 1, &[-1.0 / 5.0, 11.0 / 10.0], 0);

    let r = gauss_least_square(&a132, &b132);

    assert!(r.is_solvable);
    assert_eq!(r.rank, 2);
    assert!(Matrix::are_same(&r.x, &x_ans));
    assert_eq!(r.xs.len(), 0);
}

#[test]
fn test_gauss() {
    let m = 53; //1009;
    let n = 29; //1223;
    let mut a = Matrix::new_random(m, n, 211);
    let mut b = Matrix::new_random(m, 1, 101);

    let ans = Matrix::new_random(n, 1, 150);
    b.set_multi(&a, &ans);

    a.set_multi_val(20.0);
    b.set_multi_val(100.0);

    let r = gauss(&mut a.copy(), &mut b.copy());

    let mut x = r.x.copy();
    for xg in r.xs {
        x.accume_add(&xg);
    }

    let mut b_calced = b.copy();
    b_calced.set_val(0.0);
    b_calced.set_multi(&a, &x);

    println!("diff : {}", Matrix::diff(&b, &b_calced));
    println!("is solvable : {}", r.is_solvable);
    assert!(Matrix::are_same(&b, &b_calced));
}
