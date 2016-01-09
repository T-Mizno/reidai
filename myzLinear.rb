# load __dir__ + '/myzLinear.rb'

NUM_FORMAT = " %6.3f,"

def isZero?(x)
  x.abs < 10e-16
end

def fromTo(is, ie)
  i = is
  while i <= ie do
    yield(i)
    i = i+1
  end
end

def stdout (mat)
  mat.each do |row|
    row.each {|col| printf(NUM_FORMAT, col) }
    printf("\n")
  end
  nil
end

def stdoutXs(xs)
  if xs.length < 1 then return end
  
  for i in arrayIds(xs[0])
    xs.each do |x|
      printf(NUM_FORMAT, at(x, i, 0))
    end
    printf("\n")
  end
  nil
end

def pstdout(p, mat)
  for i in rowIds(mat)
    printf("%4d : ", p[i])
    rowValues(mat, p[i]).each {|col| printf(NUM_FORMAT, col) }
    printf("\n")
  end
  nil
end  

def newMatrix(m,n)
  mat = Array.new(m)

  for i in arrayIds(mat)
    mat[i] = Array.new(n)
  end

  matSetValue!(mat, 0.0)
  mat
end
  
def newMatrixFromArray (m,n,ls)
  mat = newMatrix(m,n)
  for i in 0...(m*n)
    set(mat, (i / n), (i % n), ls[i])
  end
  mat
end

def newSRMatrixFromArray(n,ls)
  mat = newMatrixFromArray(n,n,ls)
  for i in rowIds(mat)
    set(mat, i, i, 1.0)
    for j in (i+1)...n(mat)
      set(mat, j, i, 1.fdiv(at(mat, i, j)))
    end
  end
  mat
end

def matForeach(mat, doIJ)
  for i in rowIds(mat)
    for j in colIds(mat)
      doIJ.call(mat, i,j)
    end
  end
end

def matSetValue!(mat, val)
  matForeach(mat, lambda{|a,i,j| set(a,i,j,val)})
end

def matCopy!(mat, result)
  matForeach(result, lambda{|a,i,j| set(a, i, j, at(mat, i, j))})
end

[:matCopy].each do |f|
  define_method f do |mat|
    result = newMatrix(m(mat), n(mat))
    send((f.to_s+"!").to_sym, mat, result)
    result
  end
end

# mat{Add, Sub, EachMulti, EachDiv}Scalar!(a1, val, result)
{:+ => "Add", :- => "Sub", :* => "EachMulti", :fdiv => "Div"}.each do |op, opStr|

  ["Scalar", ""].each do |postfix|
    
    fStr = "mat"+opStr+postfix
    f = fStr.to_sym
  
    define_method (fStr+"!").to_sym do |a1, arg, result|
      if postfix=="Scalar" then
        matForeach(result, lambda{|a,i,j| set(a, i, j, at(a1, i,j).send(op, arg))})
      else
        matForeach(result, lambda{|a, i, j| set(a, i, j, at(a1,i,j).send(op, at(arg,i,j)))})
      end
    end

    define_method f do |mat, arg|
      result = newMatrix(m(mat), n(mat))
      send((f.to_s+"!").to_sym, mat, arg, result)
      result
    end
  end
end

def matMulti(a1, a2)
  result = newMatrix(m(a1), n(a2))
  matMulti!(a1, a2, result)
  result
end

def matMulti!(a1, a2, result)
  matForeach(result, lambda{|a, i, j| set(a, i, j, rowColProduct(a1, i, a2, j))})
end

def rowColProduct(a1, i, a2, j)
  colIds(a1).map{|k| at(a1, i, k) * at(a2, k, j)}.inject(:+)
end

def dotProduct(v1, v2)
  rowIds(v1).map{|i| at(v1, i, 0) * at(v2, i, 0)}.inject(:+)
end

def matDiff(a1, a2)
  tmp = 0.0
  matForeach(a1, lambda{|a,i,j| tmp = tmp + (at(a1,i,j) - at(a2, i, j))**2 })
  Math.sqrt(tmp)
end

{"1" => lambda{|x| x}, "2" => lambda{|x| x**2} }.each do |fStr, f|

  define_method "normalizeJ"+fStr+"!" do |a, j|
    deno = rowIds(a).map{|i| f.call(at(a,i,j))}.inject(:+)
    rowIds(a).map{|i| set(a, i, j, at(a,i,j).fdiv(deno))}
  end

  define_method "normalizeI"+fStr+"!" do |a, i|
    deno = colIds(a).map{|j| f.call(at(a,i,j))}.inject(:+)
    colIds(a).map{|j| set(a, i, j, at(a,i,j).fdiv(deno))}
  end

end


def at(mat, i, j)
  mat[i][j]
end

def set(mat, i, j, val)
  mat[i][j] = val
end

def setMatrix(baseMat, aI, aJ, mat)
  matForeach(mat, lambda{|a, i,j| set(baseMat, aI+i, aJ+j, at(a, i,j))})
end

# p is p[0], ..., p[M]
def pat(p, mat, i, j)
  at(mat, p[i], j)
end

def pset(p, mat, i, j, val)
  set(mat, p[i], j, val)
end

def newP(mat)
  rowIds(mat)
end

def idInMatrix?(mat, i, j)
  (iStart(mat) <= i) and (i <= iEnd(mat)) and (jStart(mat) <= j) and (j <= jEnd(mat))
end

def searchMaxI(p, mat, currentPivotI, j)
  Hash[*(range(currentPivotI, m(mat)-1).map {|i| [i, pat(p, mat, i, j).abs]}).flatten].max{|kv1, kv2| kv1[1] <=> kv2[1]}[0]
end

def canForward?(p, mat, pivot, j)
  not isZero?(pat(p, mat, pivot, j))
end

def swap!(p, oldPivot, newPivot)
  tmp = p[oldPivot]
  p[oldPivot] = p[newPivot]
  p[newPivot] = tmp
end


def forward!(p, matA, aI, aJ)
  pivotI = aI
  pivotJ = aJ
  baseJs = []

  existsPivot = false

  while true do
    
    while true do
      if not idInMatrix?(matA, pivotI, pivotJ)
      then begin
             existsPivot = false
             break
           end
      end
      newPivotI = searchMaxI(p, matA, pivotI, pivotJ)
      swap!(p, pivotI, newPivotI)
      if canForward?(p, matA, pivotI, pivotJ) 
      then begin
             existsPivot = true
             break 
           end
      end
      pivotJ = pivotJ + 1
    end

    if not existsPivot then break end

 
    pivot = pat(p, matA, pivotI, pivotJ)
    baseJs = baseJs + [pivotJ]
   for i in range(pivotI+1, m(matA)-1) do
     pset(p, matA, i, pivotJ, pat(p, matA, i, pivotJ).fdiv(pivot))
     for j in range(pivotJ+1, n(matA)-1) do
       pset(p, matA, i, j, pat(p, matA, i, j) - pat(p, matA, pivotI, j) * pat(p, matA, i, pivotJ))
     end
   end

    pivotI = pivotI + 1
    pivotJ = pivotJ + 1
  end

  freeJs = colIds(matA) - baseJs

  [baseJs, freeJs]
end

def forwardb(p, matA, b, baseJ)
  copyb = matCopy(b)
  forwardb!(p, matA, copyb, baseJ)
end

def forwardb!(p, matA, b, baseJs)
  #  for k in 0...baseJs.length
  for k in arrayIds(baseJs)    
    j = baseJs[k]
    pivotI = k
    for i in (pivotI+1)..iEnd(matA) do
      pset(p, b, i, 0, pat(p, b, i, 0) - pat(p, matA, i, j) * pat(p, b, pivotI, 0))
    end
  end
end

def backward!(p, matA, x, b, baseJs)
  for i in range(0,baseJs.length-1).reverse
    tmpb = 0.0
    #for j in (baseJs[i]+1)..jEnd(matA)
    fromTo(baseJs[i]+1, jEnd(matA)) do |j|
      tmpb = tmpb + pat(p, matA, i, j) * at(x,j,0)
    end
    set(x, baseJs[i], 0, (pat(p, b, i,0) - tmpb)/pat(p,matA, i, baseJs[i]))
  end
end

def solveX!(p, matA, x, b, baseJs)
  backward!(p, matA, x, b, baseJs)
end

def solveXs(p, matA, baseJs, freeJs)
  xs = Array.new(freeJs.length)
  b = newMatrix(m(matA), 1)
  for j in arrayIds(freeJs)
    xs[j] = newMatrix(n(matA), 1)
    set(xs[j], freeJs[j], 0, 1.0)
    #backward!(p, matA, xs[j], b, baseJs)
    solveX!(p, matA, xs[j], b, baseJs)
  end

  xs
end

def isSolvable?(p, b, baseJs)

  solvable = true
  fromTo(baseJs.length, iEnd(b)) do |i|
    solvable = solvable and isZero?(pat(p, b, i, 0))
  end
  solvable
end

def gauss(aMatA, ab)
  matA = matCopy(aMatA)
  b = matCopy(ab)
  gauss!(matA, b)
end

def gauss!(matA, b)

  p = rowIds(matA)
  x = newMatrix(n(matA), 1)

  resultForward = forward!(p, matA, 0, 0)
  baseJs = resultForward[0]
  freeJs = resultForward[1]
  forwardb!(p, matA, b, baseJs)

  solvable = isSolvable?(p, b, baseJs)

  if solvable then
    solveX!(p, matA, x, b, baseJs)
    xs = solveXs(p, matA, baseJs, freeJs)
  end

  result = GaussResult.new
  result.solvable = solvable
  result.p = p
  result.matDU = matA
  result.vecDb = b
  result.x = x
  result.xs = xs
  result.baseJs = baseJs
  result.freeJs = freeJs

  result
end

class GaussResult
  attr_accessor :x, :xs, :matDU, :vecDb, :solvable, :freeJs, :baseJs, :p

  def rank
    baseJs.length
  end

  def stdoutResult
    puts "U"
    pstdout(p, matDU)
    puts "Db"
    pstdout(p, vecDb)
    print("solvable: ", solvable, "\n")
    print("rank: ", rank(), "\n")
    print("baseJs: ", baseJs, "\n")
    print("freeJs: ", freeJs, "\n")
    if solvable then
      puts "x"
      stdout(x)
      puts "xs"
      stdoutXs(xs)
    end
  end

end


# include e(end)
def range(s, e)
  if s > e then return [] end
  Array(s..e)
end

def rowIds(mat)
  range(iStart(mat), iEnd(mat))
end

def colIds(mat)
  range(jStart(mat), jEnd(mat))
end

def arrayIds(a)
  range(0, a.length-1)
end

def m(mat)
  mat.length
end

def n(mat)
  mat[0].length
end

def iStart(mat)
  0
end

def jStart(mat)
  0
end

def iEnd(mat)
  m(mat)-1
end

def jEnd(mat)
  n(mat)-1
end

def rowVector(mat, i)
  newMatrixFromArray(1, n(mat), colIds(mat).map {|i| at(mat, i, j)})
end

def rowValues(mat, i)
  colIds(mat).map{|j| at(mat, i, j)}
end

def colVector(mat, j)
  newMatrixFromArray(m(mat), 1, rowIds(mat).map {|i| at(mat, i, j)})
end

def colValues(mat, j)
  rowIds(mat).map{|i| at(mat, i, j)}
end

def bindMatrix(a1, a2)
  mat = col(a1,0)
  for i in rowIds(mat)
    mat[i] = a1[i] + a2[i]
  end
  mat
end

def powerMethod(mat, itrMax)
  prex = colVector(mat, 0)
  x = matCopy(prex)
  error = dotProduct(x, prex)
  eigenvalue = 0.0
  itr = 1

  while (not isZero?(error)) and (itr <= itrMax)
    matCopy!(x, prex)
    matMulti!(mat, prex, x)
    eigenvalue = at(x,0,0).fdiv(at(prex, 0,0))
    normalizeJ1!(x, 0)
    error = matDiff(x, prex)
    itr = itr + 1
  end

  result = PowerResult.new
  result.eigenvector = x
  result.eigenvalue = eigenvalue
  result.error = error
  result.itr = itr

  result
end

class PowerResult
  attr_accessor :eigenvector, :eigenvalue, :itr, :error

  def stdoutResult
    print("eigenvalue: ", eigenvalue, "\n")
    print("error: ", error, "\n")
    print("itr: ", itr, "\n")
    stdout eigenvector
  end
end


def mtest()
  puts "Gauss"
  gauss($a58, $b58).stdoutResult
  puts ""
  puts "Power Method"
  powerMethod($matP, 1000).stdoutResult
end


def randomMatrix(r, m, n)
  mat = newMatrix(m, n)
  matForeach(mat, lambda{|a, i, j| set(a, i, j, r.rand)})
  mat
end



def testGauss(seed, nMax)
  r = Random.new(seed)
  m = r.rand(1..nMax)
  n = r.rand(1..nMax)
  testGauss_mn(r, m,n)
end

def testGauss_mn(r, m,n)
  matA = randomMatrix(r, m, n)
  ansX = randomMatrix(r, n, 1)
  b = matMulti(matA, ansX)
  zerob = matCopy(b)
  matSetValue!(zerob, 0.0)

#  puts "A"
#  stdout matA
#  puts "b"
#  stdout b
  result = gauss(matA, b)
  #  result.stdoutResult

  error = (matDiff(matMulti(matA, result.x), b)).fdiv(m)
  result.xs.each {|vec|
    error = error + matDiff(matMulti(matA, vec), zerob).fdiv(m)
  }

  print("seed: ", r.seed, ", error: ", error, ", rank: ", result.rank, ", m: ", m, ", n:", n,"\n")

end

$e1a = newMatrixFromArray(3,1, [
0.708,
0.291,
0.511])
$e1b = newMatrixFromArray(3,1, [
0.632,
0.260,
0.456])

$e2a = newMatrixFromArray(2,5, [
  0.495,  0.443,  0.832,  0.583,  0.025,
  0.709,  0.266,  0.264,  0.150,  0.684])
$e2b = newMatrixFromArray(2,1, [
  1.410,
  0.954])

$a20 = newMatrixFromArray(3,3, [
  2, 1, 1,
  4, 1, 0,
  -2, 2, 1
])

$b20 = newMatrixFromArray(3,1,[
  1,
  -2,
  7
 ])


$a58 = newMatrixFromArray(3,4,[
                           1, 3, 3, 2,
                           2, 6, 9, 5,
                           -1, -3, 3, 0])

$b58 = newMatrixFromArray(3,1, [1, 2, -1])

$matP = newMatrixFromArray(6,6,[
 87,  270, -12,  -49, -276,  40,
-14, -45,   6,  10,   46,  -4,
-50, -156,  4,  25,  162, -25,
94,   294, -5,  -47, -306, 49,
  1,    1,  3,   1,   0,   2,
 16,   48,  1,  -6,  -48,  8
])


$sr68 = newSRMatrixFromArray(3,[
1, 3,3,
1.0/3,1,1,
1.0/3, 1,1])

$sr69_1 = newSRMatrixFromArray(4,[
1, 3, 5, 9,
1.0/3, 1, 5,7,
1.0/5, 1.0/5, 1, 5,
1.0/9, 1.0/7, 1.0/5, 1])

$sr69_2 = newSRMatrixFromArray(4,[
1, 5.0/1, 3, 1.0/3,
5.0,1,5,3,
1.0/3, 1.0/5, 1, 1.0/3,
3,1.0/3,3,1])

$sr69_3 = newSRMatrixFromArray(4,[
1, 3, 1.0/5, 1.0/3,
1.0/3, 1, 1.0/5, 1.0/7,
5,5,1,1,
3,7,1,1])
