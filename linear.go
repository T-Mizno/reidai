package main

import "fmt"
import "os"
import "math"
import "math/rand"
import "time"

type real float64

func assert(t bool) {
	if t {
		return
	}
	fmt.Println("assert Error!")
	os.Exit(0)
}

func isZero(x real) bool { return math.Abs(float64(x)) < 10e-10 }

func foreach(ids []int, f func(i int)) {
	for _, i := range ids {
		f(i)
	}
}

// Matrix ; row size is m, column size is n.
type Matrix struct {
	m       int
	n       int
	_rowIds []int
	_colIds []int
	_data   [][]real
}

func newMatrix(m int, n int) Matrix {
	rowIds := make([]int, m)
	for i := 0; i < m; i++ {
		rowIds[i] = i
	}
	colIds := make([]int, n)
	for j := 0; j < n; j++ {
		colIds[j] = j
	}
	data := make([][]real, m)
	foreach(rowIds, func(i int) { data[i] = make([]real, n) })

	mat := Matrix{m: m, n: n, _rowIds: rowIds, _colIds: colIds, _data: data}
	mat.fill(0.0)

	return mat
}

func newMatrixFromList(m int, n int, vs []real) Matrix {
	var a = newMatrix(m, n)
	a.setFromList(vs)
	return a
}

func newMatrixCopy(aA Matrix) Matrix {
	var a = newMatrix(aA.m, aA.n)
	matCopy(aA, a)
	return a
}

func (mat Matrix) setFromList(vs []real) Matrix {
	mat.foreach(func(i int, j int) {
		if (j + i*mat.n) < len(vs) {
			mat.set(i, j, vs[j+i*mat.n])
		}
	})
	return mat
}

func randMatrix(seed int, rMin real, rMax real, m int, n int) Matrix {
	rand.Seed(int64(seed))
	var A = newMatrix(m, n)
	A.setf(func(_ int, _ int) real {
		return real(rand.Float64())*(rMax-rMin) + rMin
	})
	return A
}

func (mat Matrix) rowIds() []int { return mat._rowIds }
func (mat Matrix) colIds() []int { return mat._colIds }

func (mat Matrix) at(i int, j int) real     { return mat._data[i][j] }
func (mat Matrix) set(i int, j int, v real) { mat._data[i][j] = v }

func (mat Matrix) foreach(f func(i int, j int)) {
	foreach(mat.rowIds(), func(i int) {
		foreach(mat.colIds(), func(j int) {
			f(i, j)
		})
	})
}

func (mat Matrix) setf(f func(int, int) real) {
	mat.foreach(func(i int, j int) { mat.set(i, j, f(i, j)) })
}

func (mat Matrix) fill(v real) {
	mat.setf(func(_ int, _ int) real { return v })
}

func (mat Matrix) stdout() {
	foreach(mat.rowIds(), func(i int) {
		foreach(mat.colIds(), func(j int) {
			fmt.Printf(" %7.4f", mat.at(i, j))
		})
		fmt.Println()
	})
}

func multi(a Matrix, b Matrix, ab Matrix) {
	assert(a.n == b.m)
	assert(a.m <= ab.m)
	assert(b.n <= ab.n)

	ab.setf(func(i int, j int) real {
		s := real(0.0)
		foreach(a.colIds(), func(k int) {
			s = s + a.at(i, k)*b.at(k, j)
		})
		return s
	})
}

func multiScalar(s real, A Matrix, sA Matrix) {
	assert(sA.m >= A.m)
	assert(sA.n >= A.n)

	A.foreach(func(i int, j int) {
		sA.set(i, j, s*A.at(i, j))
	})
}

func matCopy(a Matrix, b Matrix) {
	assert(a.m <= b.m)
	assert(a.n <= b.n)

	b.setf(func(i int, j int) real { return a.at(i, j) })
}

func sumOfColumn(a Matrix, j int) real {
	sum := real(0.0)
	foreach(a.rowIds(), func(i int) {
		sum = sum + a.at(i, j)
	})
	return sum
}

func normalizeColumn(a Matrix, j int) {
	sum := sumOfColumn(a, j)
	foreach(a.rowIds(), func(i int) {
		a.set(i, j, a.at(i, j)/sum)
	})
}

func matDiff(a Matrix, b Matrix) real {
	assert(a.m == b.m)
	assert(a.n == b.n)

	d := real(0.0)
	a.foreach(func(i int, j int) {
		d = d + real(math.Abs(float64(a.at(i, j)-b.at(i, j))))
	})
	return d / real(math.Max(float64(a.m), float64(a.n)))
}

func powerMethod(a Matrix, itrMin int, itrMax int) (real, Matrix, int) {
	assert(a.m == a.n)

	x := newMatrix(a.m, 1)
	preX := newMatrix(a.m, 1)
	lambda := real(0.0)
	itr := 0

	preX.fill(1.0)

	for {
		multi(a, preX, x)
		lambda = x.at(0, 0) / preX.at(0, 0)
		normalizeColumn(x, 0)
		diff := matDiff(x, preX)
		if (itr > itrMin) && isZero(diff) {
			break
		}
		itr = itr + 1
		if itr > itrMax {
			break
		}
		matCopy(x, preX)
	}
	return lambda, x, itr
}

// GaussSystem : result of Gauss
type GaussSystem struct {
	A          Matrix
	b          Matrix
	p          []int
	x          Matrix
	rank       int
	baseVars   []int
	isSolvable bool
	solveTime  real
}

func _searchMaxIAndSwap(A Matrix, p []int, fromI int, j int) {
	ma := math.Abs(float64(A.at(p[fromI], j)))
	maI := fromI
	for i := fromI; i < A.m; i++ {
		tmp := math.Abs(float64(A.at(p[i], j)))
		if tmp > ma {
			ma = tmp
			maI = i
		}
	}
	// swap
	tmpI := p[fromI]
	p[fromI] = p[maI]
	p[maI] = tmpI
}

func _pStdout(p []int, A Matrix) {
	foreach(A.rowIds(), func(i int) {
		fmt.Printf("%4d:", i)
		foreach(A.colIds(), func(j int) {
			fmt.Printf(" %7.4f", A.at(p[i], j))
		})
		fmt.Println()
	})
}

func gauss(aA Matrix, ab Matrix) GaussSystem {
	assert(ab.m >= aA.m)

	var A = newMatrixCopy(aA)
	var p = make([]int, A.m)
	for i := 0; i < A.m; i++ {
		p[i] = i
	}
	var b = newMatrixCopy(ab)
	var x = newMatrix(aA.n, 1)
	x.fill(1.0)

	var baseVarsFlg = make([]bool, A.n)
	for j := 0; j < A.n; j++ {
		baseVarsFlg[j] = false
	}
	var isSolvable = true

	var start = time.Now()

	var pivotI, pivotJ = -1, -1
	for {
		pivotI++
		pivotJ++
		if pivotI >= A.m {
			break
		}
		if pivotJ >= A.n {
			break
		}

		_searchMaxIAndSwap(A, p, pivotI, pivotJ)
		if isZero(A.at(p[pivotI], pivotJ)) {
			continue
		}

		baseVarsFlg[pivotJ] = true
		for i := pivotI + 1; i < A.m; i++ {
			A.set(p[i], pivotJ, A.at(p[i], pivotJ)/A.at(p[pivotI], pivotJ))
			for j := pivotJ + 1; j < A.n; j++ {
				A.set(p[i], j, A.at(p[i], j)-A.at(p[i], pivotJ)*A.at(p[pivotI], j))
			}
			b.set(p[i], 0, b.at(p[i], 0)-A.at(p[i], pivotJ)*b.at(p[pivotI], 0))
		}
	}

	var baseVarsNum = 0
	var baseVars = make([]int, A.n)
	for j := 0; j < A.n; j++ {
		if baseVarsFlg[j] {
			baseVars[baseVarsNum] = j
			baseVarsNum++
		}
	}
	for i := baseVarsNum; i < A.m; i++ {
		isSolvable = isSolvable && isZero(b.at(p[i], 0)/real(A.n))
	}

	for i := baseVarsNum - 1; i >= 0; i-- {
		var sum = real(0.0)
		for j := baseVars[i] + 1; j < A.n; j++ {
			sum += A.at(p[i], j) * x.at(j, 0)
		}
		x.set(baseVars[i], 0, (b.at(p[i], 0)-sum)/A.at(p[i], baseVars[i]))
	}

	var end = time.Now()

	return GaussSystem{A: A, b: b, p: p, x: x, rank: baseVarsNum, baseVars: baseVars, isSolvable: isSolvable, solveTime: real(end.Sub(start).Seconds())}
}

func main() {

}
