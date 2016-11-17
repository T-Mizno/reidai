
def almostZero(v):
    return abs(v) < 0.00000000001

def newMatrix(m,n):
    a = list(range(0,m))
    for i in rowIds(a):
        a[i] = list(range(0,n))
    for (i,j) in ids(a):
        a[i][j] = 0.0
    return a

def newMatrixFromList(m,n,data):
    a = newMatrix(m,n)
    for (i,j) in ids(a):
        k = i*n+j
        if k < len(data):
            a[i][j] = data[i*n+j]
        else:
            a[i][j] = 0.0
    return a

def rowIds(a):
    return range(0,M(a))
def colIds(a):
    return range(0,N(a))
def ids(a):
    return range2(M(a), N(a))

class range2:
    def __init__(self, aM, aN):
        self.M = aM
        self.N = aN

    def __iter__(self):
        self.i = 0
        self.j = -1
        return self

    def __next__(self):
        self.j = self.j + 1
        if self.j >= self.N:
            self.j = 0
            self.i = self.i + 1
        if self.i >= self.M:
            raise StopIteration
        return (self.i, self.j)

def copyMatrix(a):
    mat = newMatrix(M(a), N(a))
    for (i,j) in ids(a):
        mat[i][j] = a[i][j]
    return mat

def M(a):
    return len(a)
def N(a):
    return len(a[0])
def newP(a) :
    return list(range(0,M(a)))

def stdout(a):
    for i in rowIds(a):
        print(a[i])
def stdoutP(p,a):
    for i in rowIds(a):
        print(p[i], a[p[i]])

def searchMaxIAndSwapI(p, a, pivotI, pivotJ):
    maxI = pivotI
    maxAbsValue = a[p[pivotI]][pivotJ]

    for i in range(pivotI+1, M(a)):
        if abs(a[p[i]][pivotJ]) > maxAbsValue :
            maxI = i
            maxAbsValue = abs(a[p[i]][pivotJ])

    tmpI = p[pivotI]
    p[pivotI] = p[maxI]
    p[maxI] = tmpI

def fillMatrix(a, v):
    for (i,j) in ids(a):
        a[i][j] = v

def backward(p, a, b, x, xsI, baseJs, numOfXs):
    for i in reversed(range(0, N(a)-numOfXs)):
        tmpb = 0.0
        for j in range(baseJs[i]+1, N(a)):
            tmpb += a[p[i]][j] * x[j][xsI]
        x[baseJs[i]][xsI] = (b[p[i]][0] - tmpb)/a[p[i]][baseJs[i]]

def gauss(p, a, b):
    pivotI = 0
    pivotJ = 0

    flgBaseJs = list(range(0,N(a)))
    for i in range(0,N(a)):
        flgBaseJs[i] = False

    baseJs = []

    while True :
        if pivotI >= M(a):
            break
        while True:
            if pivotJ >= N(a) :
                break
            searchMaxIAndSwapI(p, a, pivotI, pivotJ)
            if not(almostZero(a[p[pivotI]][pivotJ])) :
                break
            pivotJ = pivotJ + 1

        if pivotJ >= N(a) :
            break

        flgBaseJs[pivotJ] = True
        baseJs += [pivotJ]

        pivot = a[p[pivotI]][pivotJ]
        for i in range(pivotI+1, M(a)):
            a[p[i]][pivotJ] = a[p[i]][pivotJ]/pivot
            for j in range(pivotJ+1, N(a)):
                a[p[i]][j] = a[p[i]][j] - a[p[i]][pivotJ] * a[p[pivotI]][j]
            b[p[i]][0] = b[p[i]][0] - a[p[i]][pivotJ] * b[p[pivotI]][0]

        pivotI = pivotI + 1
        pivotJ = pivotJ + 1


    numOfXs = N(a)
    for j in flgBaseJs:
        if j :
            numOfXs = numOfXs - 1

    result = {'numOfXs': numOfXs, 'flgBaseJs': flgBaseJs, 'baseJs': baseJs}
    result['P'] = p
    result['U'] = a
    result['L-1b'] = b

    isSolvable = True
    for i in range(len(baseJs)+1, M(a)):
        isSolvable = isSolvable and almostZero(b[p[i]][0])

    result['isSolvable'] = isSolvable

    if not(isSolvable) :
        return result

    x = newMatrix(N(a), 1)
    fillMatrix(x, 0.0)
    backward(p, a, b, x, 0, baseJs, numOfXs)

    result['x'] = x

    if numOfXs < 1 :
        return result

    xs = newMatrix(N(a), numOfXs)
    fillMatrix(xs, 0.0)
    freeVarCount=0
    for j in colIds(a):
        if not(flgBaseJs[j]):
            xs[j][freeVarCount] = 1.0
            freeVarCount = freeVarCount+1
    zerob = newMatrix(M(a),1)
    fillMatrix(zerob,0.0)
    for i in range(0,numOfXs):
        backward(p, a, zerob, xs, i, baseJs, numOfXs)

    result['xs'] = xs

    return result

def stdoutGaussResult(r):
    print("U")
    stdoutP(r['P'], r['U'])
    print()
    print("baseJs", r['baseJs'], '\n')
    print("L^-1 b")
    stdoutP(r['P'], r['L-1b'])
    print()
    print("isSolvable?", r['isSolvable'])
    print("x")
    stdout(r['x'])
    print()
    print("Has Xs?", r['numOfXs'] >= 1)
    if r['numOfXs'] >= 1:
        print("Xs")
        stdout(r['xs'])
    #print(r)

A58 = newMatrixFromList(3,4,
[1,3,3,2
,2,6,9,5
,-1,-3,3,0])
b58 = newMatrixFromList(3,1,[1,2,-1])

A20 = newMatrixFromList(3,3,
[2,1,1
,4,1,0
,-2,2,1])
b20 = newMatrixFromList(3,1,[1,-2,7])


P = newP(A58)
r = gauss(P, copyMatrix(A58), copyMatrix(b58))
stdoutGaussResult(r)
