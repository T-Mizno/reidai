import random

def isZero(x):
    return abs(x) < 10e-10

def fromTo(s, e):
    return range(s, e+1)

def isMatrix(mat) :
    return (type(mat) == list) and (type(mat[0]) == list) and ( (type(mat[0][0]) == int) or (type(mat[0][0]) == float) )

def newMatrix(m,n):

    assert (m>0) and (n>0)

    return [[0 for j in range(n)] for i in range(m)]

def newMatrixFromList(m, n, ls):

    assert (m>0) and (n>0) and (len(ls) >= m*n)

    mat = newMatrix(m, n)
    for (i,j) in ids(mat) :
        lId = i * n + j
        if lId >= len(ls):
            mat[i][j] = 0.0
        else:
            mat[i][j] = ls[lId]
    return mat
                
def newSRMatrixFromList(n,ls):

    assert (n>0) and (len(ls) >= n*n)

    mat = newMatrixFromList(n,n,ls)
    for i in rowIds(mat) :
        mat[i][i] = 1.0
        for j in fromTo(i+1, jEnd(mat)) :
            mat[j][i] = 1.0 / mat[i][j]
    return mat


def matSetValue(A, val):

    assert isMatrix(A)

    for (i,j) in ids(A) :
            A[i][j] = val

def matSetMatrixRow(fromMat, fromI, toMat, toI):

    assert (iStart(fromMat) <= fromI) and (fromI <= iEnd(fromMat))
    assert (iStart(toMat) <= toI) and (toI <= iEnd(toMat))
    assert n(toMat) >= n(fromMat)

    for j in colIds(fromMat):
        toMat[toI][j] = fromMat[fromI][j]

def matSetMatrixCol(fromMat, fromJ, toMat, toJ):

    assert (jStart(fromMat) <= fromJ) and (fromJ <= jEnd(fromMat))
    assert (jStart(toMat) <= toJ) and (toJ <= jEnd(toMat))
    assert m(toMat) >= m(fromMat)

    for i in rowIds(fromMat):
        toMat[i][toJ] = fromMat[i][fromJ]

def matSetMatrix(fromMat, toMat):
    for i in rowIds(fromMat):
        matSetMatrixRow(fromMat, i, toMat, i)

def newCopyMatrix(A):
    mat = newMatrix(m(A), n(A))
    for (i,j) in ids(A):
        mat[i][j] = A[i][j]
    return mat

def stdout(mat):
    for i in rowIds(mat):
        print(mat[i], ",")

def pstdout(p, mat):
    for i in rowIds(mat):
        print(p[i], ": ", mat[p[i]])

def stdoutXs(xs):
  if len(xs) < 1 :
      return
  
  for i in rowIds(xs[0]):
      print([[xs[k][i][0]] for k in listIds(xs)])


def newP(mat):
    return list(rowIds(mat))

def m(mat):
    return len(mat)

def n(mat):
    return len(mat[0])

def iStart(mat):
    return 0

def iEnd(mat):
    return m(mat) - 1

def jStart(mat):
    return 0

def jEnd(mat):
    return n(mat)-1

def ids(mat) :
    return [(i,j) for i in rowIds(mat) for j in colIds(mat)]

def rowIds(mat):
    return fromTo(iStart(mat), iEnd(mat))

def colIds(mat):
    return fromTo(jStart(mat), jEnd(mat))

def listIds(ls):
    return fromTo(0, len(ls)-1)

def idInMatrix(mat, i, j):
    return (iStart(mat) <= i) and (i <= iEnd(mat)) and (jStart(mat) <= j) and (j <= jEnd(mat))

# DO NOT forget "abs"
def searchMaxI(p, mat, currentPivotI, j):

    maxI = currentPivotI
    maxVal = abs(mat[p[maxI]][j])

    for i in fromTo(currentPivotI+1, iEnd(mat)):
        if abs(mat[p[i]][j]) > maxVal :
            maxI = i
            maxVal = abs(mat[p[i]][j])

    return maxI

def canForward(p, mat, pivot, j):
    return not isZero(mat[p[pivot]][j])

def swap(p, oldPivot, newPivot):
  tmp = p[oldPivot]
  p[oldPivot] = p[newPivot]
  p[newPivot] = tmp

def forward(p, A, aI, aJ) :
  pivotI = aI
  pivotJ = aJ
  baseJs = []

  existsPivot = False

  while True :


      while True :
          if not idInMatrix(A, pivotI, pivotJ) :
              existsPivot = False
              break

          newPivotI = searchMaxI(p, A, pivotI, pivotJ)
          swap(p, pivotI, newPivotI)

          if canForward(p, A, pivotI, pivotJ) :
              existsPivot = True
              break 

          pivotJ = pivotJ + 1

      if not existsPivot :
          break

      pivot = A[p[pivotI]][pivotJ]
      baseJs = baseJs + [pivotJ]


      for i in fromTo(pivotI+1, iEnd(A)) :
          A[p[i]][pivotJ] = A[p[i]][pivotJ] / pivot

          for j in fromTo(pivotJ+1, jEnd(A)) :
              A[p[i]][j] = A[p[i]][j] - A[p[pivotI]][j] * A[p[i]][pivotJ]

      pivotI = pivotI + 1
      pivotJ = pivotJ + 1

  freeJs = list(set(colIds(A)) - set(baseJs))

  return [baseJs, freeJs]


def forwardb(p, A, b, baseJs):
    for k in listIds(baseJs) :
        j = baseJs[k]
        pivotI = k
        for i in fromTo(pivotI+1, iEnd(A)):
            b[p[i]][0] = b[p[i]][0] - A[p[i]][j] * b[p[pivotI]][0]


def isSolvable(p, b, baseJs):

  solvable = True

  for i in fromTo(len(baseJs), iEnd(b)) :
      solvable = solvable and isZero(b[p[i]][0])

  return solvable

def backward(p, A, x, b, baseJs):
    ls = list(listIds(baseJs)).copy()
    ls.reverse()

    for i in ls :
        tmpb = 0.0
        for j in fromTo(baseJs[i]+1, jEnd(A)) :
            tmpb = tmpb + A[p[i]][j] * x[j][0]

        x[baseJs[i]][0] = (b[p[i]][0] - tmpb) / A[p[i]][baseJs[i]]

def solveXs(p, A, baseJs, freeJs):
    xs = list(range(len(freeJs)))
    zeroVec = newMatrix(m(A), 1)
    matSetValue(zeroVec, 0.0)

    for j in listIds(freeJs):
        xs[j] = newMatrix(n(A), 1)
        matSetValue(xs[j], 0.0)
        xs[j] [freeJs[j]][0] = 1.0 
        backward(p, A, xs[j], zeroVec, baseJs)

    return xs


def gauss(A, b):

    p = newP(A)
    x = newMatrix(n(A), 1)
    xs = []

    resultForward = forward(p, A, 0, 0)
    baseJs = resultForward[0]
    freeJs = resultForward[1]
    forwardb(p, A, b, baseJs)

    solvable = isSolvable(p, b, baseJs)

    if solvable :
        backward(p, A, x, b, baseJs)
        xs = solveXs(p, A, baseJs, freeJs)

    return {"P":p, "DU":A, "Db":b, "isSolvable":solvable, "x":x, "xs":xs, "freeJs":freeJs, "baseJs":baseJs}



def rowValues(A, i) :

    assert (iStart(A) <= i) and (i <= iEnd(A))

    return [A[i][j] for j in colIds(A)]


def colValues(A, j):
    
    assert (jStart(A) <= j) and (j <= jEnd(A))

    return [ A[i][j] for i in rowIds(A)]

def matMultiRowCol(A, i, B, j):

    assert (iStart(A) <= i) and (i <= iEnd(A))
    assert (jStart(B) <= j) and (j <= jEnd(B))
    assert (n(A) <= m(B))

    return sum( [ A[i][k] * B[k][j] for k in colIds(A)] )
                      
def matSetMulti(A, B, AB):

    assert (m(A) == m(AB)) and (n(B) == n(AB))
    assert (n(A) <= m(B))

    for (i,j) in ids(AB) :
        AB[i][j] = matMultiRowCol(A,i,B,j)


def matMulti(A, B):

    assert (n(A) <= m(B))

    C = newMatrix(m(A), n(B))
    matSetMulti(A, B, C)
    return C


def dotProduct(x, y):

    assert m(x) <= m(y)

    return sum( [ x[i][0] * y[i][0] for i in rowIds(x) ] )


def matDiff(A, B):

    assert (m(A) == m(B)) and (n(A) == n(B))

    return sum([(A[i][j] - B[i][j])**2 for (i,j) in ids(A) ])


def newRandomMatrix(m, n):
    A = newMatrix(m, n)
    for i in rowIds(A):
        for j in colIds(A):
#            A[i][j] = random.random()*100
            A[i][j] = random.randint(-5, 5)+0.0

    return A

def checkGauss(seed, nMax):
    random.seed(seed)
    m = random.randint(1, nMax)
    n = random.randint(1, nMax)
    A = newRandomMatrix(m,n)
    ansX = newRandomMatrix(n, 1)
    b = matMulti(A, ansX)
    zeroVec = newCopyMatrix(b)
    matSetValue(zeroVec, 0.0)

    result = gauss(newCopyMatrix(A), newCopyMatrix(b))
    error = matDiff(matMulti(A, result["x"]), b)
    for k in listIds(result["xs"]):
        error = error + matDiff(matMulti(A, result["xs"][k]), zeroVec)

    rank = len(result["baseJs"])

#    if (rank != m) and (rank != n) :
#    if not isZero(error) :
    print({"seed":seed, "m":m, "n":n, "rank":len(result["baseJs"]), "error":error})
#    stdout(A)
#    stdout(b)


def gtest(n):
    for k in fromTo(1, 100):
        checkGauss(k, n)

def ptest(matP):
    r = powerMethod(matP, 100)
    print("eigenvalue: ", r["eigenvalue"], ", itr: ", r["itr"], ", error: ", r["error"])
    stdout(r["eigenvector"])

def normalizeJ1(A, j):
    deno = 0.0
    for i in rowIds(A):
        deno = deno + abs(A[i][j])
    for i in rowIds(A):
        A[i][j] = A[i][j]/deno
        
def powerMethod(A, itrMax):
    prex = newMatrix(n(A), 1)
    matSetMatrixCol(A, 0, prex, 0)
    x = newCopyMatrix(prex)
    error = dotProduct(x, prex)
    eigenvalue = 0.0
    itr = 1

    while (not isZero(error)) and (itr <= itrMax) :
        matSetMatrix(x, prex)
        matSetMulti(A, prex, x)
        eigenvalue = x[0][0] / prex[0][0]
        normalizeJ1(x, 0)
        error = matDiff(x, prex)
        itr = itr + 1

    return {"itr":itr, "error":error, "eigenvector":x, "eigenvalue":eigenvalue}



A20 = newMatrixFromList(3,3, [
        2, 1, 1,
        4, 1, 0,
        -2, 2, 1
        ])

b20 = newMatrixFromList(3,1,[
        1,
        -2,
        7
        ])


A58 = newMatrixFromList(3,4,[
        1, 3, 3, 2,
        2, 6, 9, 5,
        -1, -3, 3, 0
        ])

b58 = newMatrixFromList(3,1, [1, 2, -1])

P1 = newMatrixFromList(6,6,[
        87,  270, -12,  -49, -276,  40,
        -14, -45,   6,  10,   46,  -4,
        -50, -156,  4,  25,  162, -25,
        94,   294, -5,  -47, -306, 49,
        1,    1,  3,   1,   0,   2,
        16,   48,  1,  -6,  -48,  8
        ])

SR68 = newSRMatrixFromList(3,[
1, 3,3,
1.0/3,1,1,
1.0/3, 1,1])

SR69_1 = newSRMatrixFromList(4,[
1, 3, 5, 9,
1.0/3, 1, 5,7,
1.0/5, 1.0/5, 1, 5,
1.0/9, 1.0/7, 1.0/5, 1])

SR69_2 = newSRMatrixFromList(4,[
1, 5.0/1, 3, 1.0/3,
5.0,1,5,3,
1.0/3, 1.0/5, 1, 1.0/3,
3,1.0/3,3,1])

SR69_3 = newSRMatrixFromList(4,[
1, 3, 1.0/5, 1.0/3,
1.0/3, 1, 1.0/5, 1.0/7,
5,5,1,1,
3,7,1,1])



#seed 981
Ae1 = [
[-3.0],
[0.0]
]

be1 = [
[0.0],
[0.0]
]

#seed 96, 20 12x11
Ae2 = [
[5.0, 1.0, -2.0, -4.0, 0.0, -5.0, -5.0, -3.0, 1.0, 5.0, 0.0] ,
[-4.0, -1.0, 0.0, 0.0, -1.0, -3.0, 3.0, -5.0, 4.0, 2.0, 3.0] ,
[2.0, 2.0, 0.0, -2.0, 0.0, 3.0, 3.0, 0.0, -5.0, -3.0, -5.0] ,
[-4.0, 2.0, 3.0, -2.0, 0.0, -4.0, -3.0, 4.0, -4.0, 3.0, -1.0] ,
[4.0, 5.0, -1.0, -1.0, 5.0, -2.0, 1.0, 1.0, 4.0, -5.0, -1.0] ,
[3.0, 0.0, 5.0, 4.0, 5.0, 1.0, 1.0, -4.0, 4.0, 4.0, -2.0] ,
[2.0, 0.0, -2.0, -5.0, 4.0, -1.0, -3.0, -2.0, 4.0, -3.0, -1.0] ,
[1.0, -5.0, 2.0, -3.0, -3.0, -4.0, -5.0, 2.0, -1.0, 5.0, -5.0] ,
[-3.0, 5.0, -5.0, 0.0, -3.0, 5.0, -4.0, 3.0, -1.0, -2.0, -3.0] ,
[-3.0, -5.0, -4.0, -2.0, 0.0, -1.0, 5.0, -4.0, 3.0, 0.0, -1.0] ,
[-4.0, -2.0, 4.0, 4.0, -3.0, 2.0, -1.0, -3.0, 0.0, 5.0, -3.0] ,
[-5.0, -4.0, 3.0, 2.0, 0.0, 1.0, -3.0, -5.0, -2.0, -5.0, 1.0] 
]
be2 = [
[-8.0] ,
[41.0] ,
[-6.0] ,
[-81.0] ,
[83.0] ,
[9.0] ,
[56.0] ,
[-126.0] ,
[6.0] ,
[33.0] ,
[-67.0] ,
[-1.0]
]
