package main

import "testing"

//import "math"
import "time"

/*
$ golint linear.go
$ golint linear_test.go
$ go test -v linear_test.go linear.go
*/
func howLongSeconds(f func()) real {
	var start = time.Now()
	f()
	var end = time.Now()
	return real(end.Sub(start).Seconds())
}

func TestMulti(t *testing.T) {
	var A = newMatrix(2, 3)
	var B = newMatrix(3, 4)
	var AB = newMatrix(2, 4)
	var ABans = newMatrix(2, 4)

	A.setFromList([]real{
		1, 2, 3,
		4, 5, 6,
	})
	B.setFromList([]real{
		1, 2, 3, 1,
		2, 3, 4, 1,
		3, 4, 5, 5,
	})
	ABans.setFromList([]real{
		14, 20, 26, 18,
		32, 47, 62, 39,
	})

	multi(A, B, AB)

	if !isZero(matDiff(AB, ABans)) {
		t.Errorf("error in multi")
	}
}

func TestPowerMethod(t *testing.T) {
	var n = 100
	var eigen = newMatrix(n, 1)
	var lambda real
	var itr int
	var d real
	var x = newMatrix(n, 1)

	var rMax = real(10)

	var A = randMatrix(10000, 1, rMax, n, n)

	lambda, eigen, itr = powerMethod(A, 10, 100)

	multi(A, eigen, x)
	multiScalar(lambda, eigen, eigen)

	d = matDiff(eigen, x) / 2 / rMax
	t.Log("itr: ", itr)
	t.Log("diff: ", d)

	if !isZero(d) {
		t.Errorf("error in power method")
	}
}

func TestGaussA20(t *testing.T) {
	var A20 = newMatrixFromList(3, 3, []real{
		2, 1, 1,
		4, 1, 0,
		-2, 2, 1,
	})
	var b20 = newMatrixFromList(3, 1, []real{1, -2, 7})
	var ans = newMatrixFromList(3, 1, []real{-1, 2, 1})
	var gs = gauss(A20, b20)
	var d = matDiff(gs.x, ans)
	if !isZero(d) {
		t.Errorf("error: gauss: A20")
	}
}

func TestGaussA58(t *testing.T) {
	var A = newMatrixFromList(3, 4, []real{
		1, 3, 3, 2,
		2, 6, 9, 5,
		-1, -3, 3, 0,
	})
	var b = newMatrixFromList(3, 1, []real{1, 2, -1})
	var ans = newMatrixFromList(4, 1, []real{-3, 1, -1.0 / 3.0, 1})
	var gs = gauss(A, b)
	var d = matDiff(gs.x, ans)
	if !isZero(d) {
		t.Errorf("error: gauss: A58")
	}
}

func TestGauss(t *testing.T) {
	var m = 500
	var n = 500
	var rMax real = 100
	var A = randMatrix(1000, -rMax, rMax, m, n)
	var ans = randMatrix(101, -rMax, rMax, n, 1)
	var b = newMatrix(m, 1)
	multi(A, ans, b)

	gs := gauss(A, b)

	var bTmp = newMatrix(m, 1)
	multi(A, gs.x, bTmp)

	var d = matDiff(b, bTmp) / 2 / rMax
	t.Log("diff: ", d)
	t.Log("rank:", gs.rank)
	t.Log("time: ", gs.solveTime)

	if !gs.isSolvable {
		t.Errorf("error: gauss: solvable")
	}
	if !isZero(d) {
		t.Errorf("error: gauss: x")
	}
}
