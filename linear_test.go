package main

import "testing"
import "math"
import "time"

/*
$ golint linear.go
$ golint linear_test.go
$ go test -v linear_test.go linear.go
*/
func howLongSeconds(f func()) real {
		var start = time.Now()
		f()
		var end=time.Now()
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

	var A = randMatrix(10000, -1, 10, n, n)

	lambda, eigen, itr = powerMethod(A, 100)

	multi(A, eigen, x)
	multiScalar(lambda, eigen, eigen)

	d = matDiff(eigen, x)
	t.Log("itr: ", itr)
	t.Log("diff: ", d)

	if d > 0.00001 {
		t.Errorf("error in power method")
	}
}

func TestGaussA20(t *testing.T) {
	var A20 = newMatrix(3, 3)
	A20.setFromList([]real {
		2, 1, 1,
	 	4,1,0,
		-2, 2, 1,
	})
	var b20 = newMatrix(3, 1)
	b20.setFromList([]real { 1, -2, 7})
	var ans = newMatrix(3,1)
	ans.setFromList([]real { -1, 2, 1})
	var x, _, _ = gauss(A20, b20)
	var d = matDiff(x, ans)
	if !isZero(d) {
		t.Errorf("error: gauss: A20")
	}
}

func TestGaussA58(t *testing.T) {
	var A = newMatrix(3, 4)
	A.setFromList([]real {
		1,3,3,2,
		2,6,9,5,
		-1, -3, 3, 0,
	})
	var b = newMatrix(3, 1)
	b.setFromList([]real { 1, 2, -1})
	var ans = newMatrix(4,1)
	ans.setFromList([]real { -3, 1, -1.0/3.0, 1})
	var x, _, _ = gauss(A, b)
	var d = matDiff(x, ans)
	if !isZero(d) {
		t.Errorf("error: gauss: A58")
	}
}

func TestGauss(t *testing.T) {
	var m = 200
	var n = 200
	var rMax real = 100
	var A = randMatrix(1000, -rMax, rMax, m, n)
	var ans = randMatrix(101, -rMax, rMax, n, 1)
	var b = newMatrix(m, 1)
	var x Matrix
	var rank int
	var solvable bool
	multi(A,ans, b)

	x, rank, solvable = gauss(A, b)

	var bTmp = newMatrix(m, 1)
	multi(A, x, bTmp)

	var d = matDiff(b, bTmp)/2/rMax/real(math.Sqrt(float64(m*n)))
	t.Log("diff div by order: ", d)
	t.Log("rank:", rank)

	if !solvable {
		t.Errorf("error: gauss: solvable")
	}
	if d > 0.0000001 {
		t.Errorf("error: gauss: x")
	}
}
