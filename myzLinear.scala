// 
// Gaussian elimination
//    Mizno (of.mizno@gmail.com)
//  2013/10/10
//  2013/10/16 
//       repaire 'searchMaxi' to compare abs values.
//       calculation eigenValue at 'powerMethod'
//  2013/10/24
//       検証用のメソッドを追加
//  2013/11/12
//       myzLinear パッケージに変更
//       Num -> Double に変更
//  2013/11/15
//       コレクションにインデックスアクセスしている部分が特に重かったので、while文に置き換えた
//       　　(実行時間を1/10倍以下に)
//       畳み込み(/:)をwhile文に置き換えた
//

// Usage (for Windows) :
//  mkdir classes
//  scalac -d .\classes myzLinear.scala
//  scala -classpath .\classes
//  scala> myzLinear.Test.itr_check(2)


package myzLinear

//type Num=Double
//class Double extends Double

class Matrix(val iStart:Int, val iEnd:Int, val jStart:Int, val jEnd:Int, val default:Double){

  val m = iEnd - iStart + 1
  val n = jEnd - jStart + 1
  val _values = Array.fill[Double] (m, n) { default }

  val rowIndices = iStart to iEnd
  val colIndices = jStart to jEnd
  val indices = for(i <- rowIndices; j <- colIndices) yield (i,j)

  def idInRange(i:Int, j:Int):Boolean = (iStart <= i) && (i <= iEnd) && (jStart <= j) && (j <= jEnd)
  def idInRange(id:(Int, Int)):Boolean = idInRange(id._1, id._2)

  def i2zi(i:Int):Int = { i - iStart  }
  def j2zj(j:Int):Int = { j - jStart  }

  def id2sid(id:(Int, Int)):Int = {
    val i = id._1
    val j = id._2
    (i - iStart) * n + (j - jStart)
  }
  def sid2id(sid:Int) : (Int, Int) = {
    val i = (sid / n) + iStart
    val j = (sid % n) + jStart
    (i, j)
  }

  def at(i:Int, j:Int): Double = {
    require( idInRange(i,j) )

    _values(i2zi(i))(j2zj(j))
  }
  def at(idx:(Int, Int)): Double = at(idx._1, idx._2)

  def set(i:Int, j:Int, v: Double) {
    if( idInRange(i, j) ) _values(i2zi(i))(j2zj(j)) = v
  }
  def set( idx:(Int, Int), v:Double ) {
    set(idx._1, idx._2, v)
  }
  def set(aValues:Seq[((Int,Int), Double)]) {
    aValues.foreach( v => set(v._1, v._2))
  }

  def this(aM:Int, aN:Int, aDefault:Double) ={
    this(0, aM - 1, 0, aN -1, aDefault)
  }
  def this(aM:Int, aN:Int) = this (aM, aN, 0)
  def this(am:Int, an:Int, aValues:Seq[Double]) = {
    this(am, an)

    indices.foreach(idx =>  
		    if(id2sid(idx) < aValues.size)
		      set(idx, aValues(id2sid(idx))))
  }

  def this(aiStart:Int, aiEnd:Int, ajStart:Int, ajEnd:Int) = {
    this(aiStart, aiEnd, ajStart, ajEnd, 0.0)
  }
  def this(aiStart:Int, aiEnd:Int, ajStart:Int, ajEnd:Int, aValues:Seq[Double]) = {
    this(aiStart, aiEnd, ajStart, ajEnd, 0.0)
    fillList(aValues)
  }
  def this(aValues:Seq[((Int, Int), Double)]) = {
    this(aValues.map(_._1._1).min,
	 aValues.map(_._1._1).max,
	 aValues.map(_._1._2).min,
	 aValues.map(_._1._2).max)
    set(aValues)
  }

  override def toString:String = {
    var str = ""
    str = str + "    "
    for(j<-colIndices) str = str + "  " + i2Str( j)
    str = str + "\n"
    for(i <- rowIndices) {
      str = str + i2Str(i)+":  " + elem2Str(at(i, colIndices.head))
      for(j <- colIndices.tail) {
	str = str + ", " + elem2Str(at(i,j))
	}
      str = str + "\n"
    }
    str
  }
  def i2Str :Int => String = x => {
    if(x.isInstanceOf[Int]) "%5d".format(x)
    else x.toString
  }
  def elem2Str : Double => String =  x => {
    if(x.isInstanceOf[Double]  ||  x.isInstanceOf[Float]) "%5.2f".format(x)
    else x.toString
  }

  def setPartial(si:Int, sj:Int, mat:Matrix) {
    mat.indices.foreach(idx => set(si+idx._1, sj+idx._2, mat.at(idx)))
  }

  def fill(v:Double) {
    indices.foreach(idx => set(idx, v))
  }
  def fillList(aValues:Seq[Double]) {
//    (0 to Math.min(aValues.size - 1, m * n -1)).foreach(sid => set(sid2id(sid), aValues(sid))) // 処理が重い
    var vs = aValues
    var ids = indices
    while (!(vs.isEmpty || ids.isEmpty)) {
      set(ids.head, vs.head)
      vs = vs.tail
      ids = ids.tail
    }
  }

  def copy:Matrix = new Matrix (
    indices.map(idx => (idx, at(idx)))
  )

  def copyTo(that:Matrix)  {
    indices.foreach(idx => that.set(idx, at(idx)))
    
    /*
    var ids = indices
    while(! ids.isEmpty) {
      var idx = ids.head
      that.set(idx, at(idx))
      ids = ids.tail
    }
    */
  }

  def +(that:Matrix):Matrix = {
    var mat = new Matrix(this.iStart, this.iEnd, this.jStart, this.jEnd)
    setAdd(that, mat)
    mat
  }
  def +(v:Double):Matrix = {
    var mat = copy
    mat.setThisAdd(v)
    mat
  }
  def setAdd(that:Matrix, target:Matrix) {
    indices.foreach(idx => 
      if(that.idInRange(idx))
	target.set(idx, at(idx)+that.at(idx)))
  }
  def setAdd(v:Double, target:Matrix) {
    indices.foreach(idx => target.set(idx, at(idx)+v))
  }
  def setThisAdd(v:Double) {
    indices.foreach(idx => set(idx, at(idx)+v))
  }

  def -(that:Matrix):Matrix = {
    var mat = new Matrix(m, n)
    setSub(that, mat)
    mat
  }
  def setSub(that:Matrix, target:Matrix) {
    indices.foreach(idx => 
      if(that.idInRange(idx))
	target.set(idx, at(idx)-that.at(idx)))
  }

  def *(that:Matrix):Matrix = {
    var mat = new Matrix(this.iStart, this.iEnd, that.jStart, that.jEnd)
    setMultiply(that, mat)
    mat
  }
  def setMultiply(that:Matrix, target:Matrix) {
    require( colIndices == that.rowIndices )

    for(i <- rowIndices) {
      for(j <- that.colIndices) {
        var inner = 0:Double
        for(k <- colIndices) {
          inner = inner + at(i,k) * that.at(k,j)
        }
        target.set(i,j, inner)
      }
    }
  }

  def /(deno:Double):Matrix = {
    val mat = copy
    mat.setThisDiv(deno)
    mat
  }
  def divColumn(j:Int, deno:Double):Matrix = {
    val mat = copy
    mat.setThisDivColumn(j, deno)
    mat
  }
  def setThisDivColumn(j:Int, deno:Double) {
    for(i <- rowIndices) set(i,j, at(i,j)/deno)
  }

  def divRow(i:Int, deno:Double):Matrix ={
    val mat = copy
    mat.setThisDivRow(i, deno)
    mat
  }
  def setThisDivRow(i:Int, deno:Double){
    colIndices.foreach(j => set(i,j, at(i,j)/deno))
  }

  def setThisDiv(deno:Double) {
    for(j <- colIndices) setThisDivColumn(j, deno)
  }


  def sumOfColumn(j:Int):Double = {
    require( colIndices.contains(j) )
    //    ((0:Double) /: rowIndices) ((v, i) => v + at(i, j))
    norm1OfColumn(j)
  }
  def sumOfRow(i:Int):Double = {
    require( rowIndices.contains(i) )
    ((0:Double) /: colIndices) ((v, j) => v + at(i, j))
  }

  def norm1OfColumn(j:Int):Double = {
    require( colIndices.contains(j) )
    ((0:Double) /: rowIndices) ((v, i) => v + Math.abs(at(i, j)))
  }
  def norm2OfColumn(j:Int):Double = {
    require( colIndices.contains(j) )
    Math.sqrt( ((0:Double) /: rowIndices) ((v, i) => v + Math.pow(at(i, j), 2)))
  }

  def normalizeColumn(j:Int):Matrix = {
    val mat = copy
    mat.setThisNormalizeColumn(j)
    mat
  }
  def setThisNormalizeColumn(j:Int) {
    setThisDivColumn(j, sumOfColumn(j))
  }

  def normalizeRow(i:Int):Matrix = {
    val mat = copy
    mat.setThisNormalizeRow(i)
    mat
  }
  def setThisNormalizeRow(i:Int) {
    setThisDivRow(i, sumOfRow(i))
  }

  def averageColumn : Matrix = {
    new Matrix( rowIndices.map(i => ((i,0), sumOfRow(i)/n)))
  }
  def averageRow : Matrix = {
    new Matrix( colIndices.map(j => ((0,j), sumOfColumn(j)/m)))
  }


  def rowDiagMatrix(i:Int):Matrix = {
    var mat = new Matrix(jStart, jEnd, jStart, jEnd, 0.0)
    for(j <- 0 until n) mat.set(j,j, at(i,j))
    mat
  }
  def rowDiagInverseMatrix(i:Int):Matrix = {
    var mat = rowDiagMatrix(i)
    for(j <- 0 until n) mat.set(j,j, 1.0/at(i,j))
    mat
  }
  def normalizeThisByDiagInverse(i:Int) {
    if((0 until m).contains(i)) {
      val result = this * rowDiagInverseMatrix(i)
      result.copyTo(this)
    }
  }


  def transpose : Matrix = new Matrix(indices.map(id => ((id._2,id._1), at(id._1,id._2))))


  def ==(that:Matrix):Boolean = equals(that)
  def equals(that:Matrix):Boolean = {
    (m == that.m) && (n == that.n) && Util.isZero(error(that))
  }
  def error(that:Matrix):Double = {
    val s = Math.sqrt( ( (0:Double) /: indices.map(idx => Math.pow(at(idx) - that.at(idx), 2.0)) ) (_ + _) )
    s / m / n
  }

  def rowValues(i:Int):Seq[Double] = {
    require( rowIndices.contains(i) )
    colIndices.map(j => at(i,j))
  }
  def colValues(j:Int):Seq[Double] = {
    require( colIndices.contains(j) )
    rowIndices.map(i => at(i,j))
  }

}


object Util {
  val epsilon = 1e-6:Double

  def near(x:Double, y:Double):Boolean = Math.abs(x - y) < epsilon
  def isZero(x:Double):Boolean = near(x, 0.0)

  //
  // gaussian elimination
  //
  def newP (A:Matrix) = {
    val p = scala.collection.mutable.Map.empty[Int, Int]
    A.rowIndices.foreach(i => p += (i -> i))
    p
  }
  def swapP(p:scala.collection.mutable.Map[Int, Int], i1:Int, i2:Int) {
    val tmpi1 =  p(i1)
    p.update(i1, p(i2))
    p.update(i2, tmpi1)
  }

  def searchMaxI(p:scala.collection.mutable.Map[Int, Int], A:Matrix, fromi:Int, j:Int):Int = {
    var maxi = fromi
    var maxv = A.at(p(fromi) ,j)
		   (fromi to A.iEnd).foreach( i =>
      if(A.at(p(i), j) > maxv) {
	maxi = i
	maxv = A.at(p(i),j)
      }
    )
    maxi
  }

  def updateP(p:scala.collection.mutable.Map[Int, Int], A:Matrix, pi:Int, pj:Int) {
    swapP(p, pi, searchMaxI(p, A, pi, pj))
  }

  def underColumnsAreZero(p:scala.collection.mutable.Map[Int, Int], A:Matrix, fromi:Int, j:Int) : Boolean = {
    ( (fromi to A.iEnd).map(i => A.at(p(i), j)) ).forall(Util.isZero(_))
  }

  def searchPivotj(p:scala.collection.mutable.Map[Int, Int], A:Matrix, pi:Int, j:Int) : Option[Int]  = {
    if(! A.idInRange(pi, j)) return None

    updateP(p, A, pi, j)
    if (underColumnsAreZero(p, A, pi, j)) 
      searchPivotj(p, A, pi, (j + 1))
    else
      Some(j)
  } 

  def forwardOneLine(p:scala.collection.mutable.Map[Int, Int], A:Matrix, b:Matrix, pi:Int, pj:Int, i:Int) {
    val pci = p(i)
    val pvi = p(pi)
    val pv = A.at(pci, pj) / A.at(pvi, pj)


    ((pj + 1) to A.jEnd).foreach(j =>
      A.set(pci, j, A.at(pci, j) - (pv * A.at(pvi, j))))

    /*
    var j = pj + 1
    while(j <= A.jEnd) {
      A.set(pci, j,    A.at(pci, j) - (pv * A.at(pvi, j)))
      j = j + 1
    }
    *
    */

    b.set(pci, 0,
	  b.at(pci,0) - (pv * b.at(pvi, 0)))
    A.set(pci, pj, pv)
  }
  def forwardRows(p:scala.collection.mutable.Map[Int, Int], 
		  A:Matrix, b:Matrix, pi:Int, pj:Int) {

    ((pi + 1) to A.iEnd).foreach( i => forwardOneLine(p, A, b, pi, pj, i) )

    /*
    var i = pi + 1
    while(i <= A.iEnd) {
      forwardOneLine(p, A, b, pi, pj, i)
      i = i + 1
    }
    */
  }

  def forwardOld(p:scala.collection.mutable.Map[Int, Int], 
	      A:Matrix, b:Matrix, pi:Int, j:Int,
	      pivots: scala.collection.mutable.Map[Int, Int]) {
    var pj = j
    if(! A.idInRange(pi, pj)) return ()
    searchPivotj(p, A, pi, pj) match {
      case Some(newPj) => pj = newPj
      case _ => return //pivots
    }
    forwardRows(p, A, b, pi, pj)
    pivots += (pi -> pj)
    forward(p, A, b, (pi + 1), (pj + 1), pivots)
  }

  def forward(p:scala.collection.mutable.Map[Int, Int], 
	      A:Matrix, b:Matrix, api:Int, aj:Int,
	      pivots: scala.collection.mutable.Map[Int, Int]) {
    var j = aj
    var pi = api
    var pj = j
    while(A.idInRange(pi, pj)) {
      searchPivotj(p, A, pi, pj) match {
	case Some(newPj) => pj = newPj
	case _ => return //pivots
      }
      forwardRows(p, A, b, pi, pj)
      pivots += (pi -> pj)
      pi = pi + 1
      pj = pj + 1
    }
  }

  def freeColIndices(A:Matrix, pivots: scala.collection.mutable.Map[Int, Int]) : Seq[Int] = {
    A.colIndices.diff(pivots.values.toList)
  }
  def baseColIndices(pivots: scala.collection.mutable.Map[Int, Int]) : Seq[Int] = {
    pivots.values.toList
  }
  def freeRowIndices(A:Matrix, pivots: scala.collection.mutable.Map[Int, Int]) : Seq[Int] = {
    A.rowIndices.diff(pivots.keys.toList)
  }
  def baseRowIndicies(pivots: scala.collection.mutable.Map[Int, Int]) : Seq[Int] = {
    pivots.keys.toList
  }
  def pivotj2i(pivots: scala.collection.mutable.Map[Int, Int], j:Int):Int = {
    pivots.keys.toList.filter(pivots(_) == j).head
  }

  def solvable(p:scala.collection.mutable.Map[Int, Int], A:Matrix, b:Matrix, pivots:scala.collection.mutable.Map[Int, Int]) : Boolean = {
    freeRowIndices(A, pivots).forall(i => Util.isZero(b.at(p(i), 0)))
  }

  def backwardOneLine(p:scala.collection.mutable.Map[Int, Int], A:Matrix, i:Int, j:Int,
		      argb:Matrix, argx:Matrix) {
    val pci = p(i)

    /*
    argx.set(j,0,
	     (argb.at(pci,0) - ( (0.0:Double)  /: ((j + 1) to A.jEnd).map( k => A.at(pci,k) * argx.at(k, 0))) (_ + _))/A.at(pci,j)
	   )
	   */
    var k = j + 1
    var sum = 0.0
    while(k <= A.jEnd) {
      sum = sum + A.at(pci,k) * argx.at(k, 0)
      k = k + 1
    }
    argx.set(j,0,    (argb.at(pci,0) -  sum) /A.at(pci,j)  )
  }

  def backward(p:scala.collection.mutable.Map[Int, Int], A:Matrix,
	       argb:Matrix, argx:Matrix, pivots:scala.collection.mutable.Map[Int, Int]) {
    baseColIndices(pivots).sorted.reverse.foreach(j =>
      backwardOneLine(p, A, pivotj2i(pivots, j), j, argb, argx))

    /*
    var bis = baseColIndices(pivots).sorted.reverse
    while(! bis.isEmpty) {
      val j = bis.head
      backwardOneLine(p, A, pivotj2i(pivots, j), j, argb, argx)
      bis = bis.tail
    }
    */
  }

  def gauss(A:Matrix, b:Matrix) : (Boolean, Map[Int, Int], Map[Int, Int], Matrix, Matrix, Matrix, Seq[Matrix]) = {
    require( A.rowIndices == b.rowIndices )
    val DU = A.copy
    val DUb = b.copy
    val x = A.averageRow.transpose
    x.fill(0.0)
    val p = newP(A)
    val pivots = scala.collection.mutable.Map.empty[Int, Int]
    
    val xs = new scala.collection.mutable.ListBuffer[Matrix]

    var pi = A.iStart
    var pj = A.jStart

    forward(p, DU, DUb, pi, pj, pivots)
    if(! solvable(p, DU, DUb, pivots)) {
      return (false,
	      p.toMap,
	      pivots.toMap,
	      DU,
	      DUb,
	      x,
	      xs.toList)
      
    }
    
    // backward for special solution
    backward(p, DU, DUb, x, pivots)

    // backward for general solutions
    freeColIndices(A, pivots).foreach(k =>
      {
	val tmpx = x.copy
	tmpx.fill(0.0)
	tmpx.set(k, 0, 1.0)
	val tmpb = b.copy
	tmpb.fill(0.0)

	backward(p, DU, tmpb, tmpx, pivots)
	xs += tmpx
      }
    )
    /*
    var fcis = freeColIndices(A, pivots)
    while(! fcis.isEmpty) {
      val k = fcis.head
      
      val tmpx = x.copy
      tmpx.fill(0.0)
      tmpx.set(k, 0, 1.0)
      val tmpb = b.copy
      tmpb.fill(0.0)

      backward(p, DU, tmpb, tmpx, pivots)
      xs += tmpx

      fcis = fcis.tail
    }
    */
    
    (true,
     p.toMap,
     pivots.toMap,
     DU,
     DUb,
     x,
     xs.toList)
  }

  //
  // rank of matrix
  //
  def rank(mat:Matrix) : Int = {
    val result = gauss(mat, mat.averageColumn)
    (result._3).size
  }

  //
  // least square method by gaussian elimination
  //
  def leastSquare(A:Matrix, b:Matrix) : (Boolean, Map[Int, Int], Map[Int, Int], Matrix, Matrix, Matrix, Seq[Matrix]) = {
    gauss(A.transpose * A,  A.transpose * b)
  }

  //
  // power method
  //
  def powerMethod(mat:Matrix, maxCount:Int) :(Int, Double, Matrix) = {
    require(mat.rowIndices == mat.colIndices)

    var x = mat.averageRow.transpose
    x.fill(1.0)
    var preX = mat.averageRow.transpose

    var count:Int = 0
    do {
      x.copyTo(preX)
      mat.setMultiply(preX, x)
      x.setThisNormalizeColumn(0)
      count = count + 1
    }while( (! x.equals(preX)) && (count < maxCount) )
    println("power method itr: " + count)
    mat.setMultiply(x, preX)
    (count, preX.sumOfColumn(0)/x.sumOfColumn(0), x)
  }

  //
  // geometric mean method
  //
  def geomMeanMethod(mat:Matrix) :(Double, Matrix) = {
    require(mat.rowIndices == mat.colIndices)
    var x = mat.averageRow.transpose

    mat.rowIndices.foreach(i => x.set(i,0, Math.pow( ((1.0:Double) /: mat.rowValues(i))(_ * _), 1.0/mat.m)) )
    x.setThisNormalizeColumn(0)

    var tmpX = mat * x

    ((((0:Double) /: mat.rowIndices.map(i => (tmpX.at(i,0)/x.at(i,0)))) (_ + _))/mat.m, x)
  }

}

object Test {

  //時間計測
  def time(process: => Unit) = {
    val start = System.currentTimeMillis
    process
    println("Time:" + (System.currentTimeMillis - start) + "msec")
  }

  // 
  // Sample matrix
  //
  def matA58 = new Matrix (-1, 1, -2, 1, Seq(
1.0, 3.0, 3.0, 2.0,
2.0, 6.0, 9.0, 5.0,
-1.0, -3.0, 3.0, 0.0))
  def b58 = new Matrix (-1, 1, 0, 0, Seq (1.0, 2.0, -1.0))
  def b58b = new Matrix (-1, 1, 0, 0, Seq (1.0, 2.0, -1.5))

  def matA20 = new Matrix (-1, 1, -2, 0, Seq(
2.0, 1.0, 1.0,
4.0, 1.0, 0.0,
-2.0, 2.0, 1.0))
  def b20 = new Matrix (-1, 1, 0, 0, Seq(1.0, -2.0,  7.0))

  def matA132 = new Matrix (-2, 1, 3, 4, Seq(
1.0, 0.0,
1.0, 1.0,
1.0, 3.0,
1.0, 4.0))
  def b132 = new Matrix (-2, 1, 0, 0, Seq(0.0, 1.0, 2.0, 5.0))

  def matP:Matrix = new Matrix(3, 3,List(
    1.0, 0.5, 3.0,
    2.0, 1.0, 0.6,
    (1.0/3.0), (1.0/0.6), 1.0
  ))

  def matP2 = new Matrix (6,6, List(
    87.0, 270.0, -12.0, -49.0, -276.0,  40.0,
    -14.0, -45.0,   6.0,  10.0,   46.0,  -4.0,
    -50.0, -156.0,  4.0,  25.0,  162.0, -25.0,
    94.0,   294.0, -5.0,  -47.0, -306.0, 49.0,
    1.0,    1.0,  3.0,   1.0,   0.0,   2.0,
    16.0,   48.0,  1.0,  -6.0,  -48.0,  8.0
  ))


  // for test
  import scala.util.Random

  val tolerantSize = 100
  val randIntMax = 10000
  val randDoubleMax = 1000.0

  def isSufficientZero(v:Double) : Boolean = Math.abs( v - 0.0) < 1e-5

  def randSign : Int = {
    Random.shuffle( List(-1,1)).head
  }
  def errorXs(a:Matrix, xs:Seq[Matrix]): Double = {
    if(xs.length > 0)
      ( ((0.0:Double) /: xs )( (acc,x) => (acc + (a * x).sumOfColumn(0)))) / xs.length / a.m / a.n
    else
      0.0
  }

    
  def seed_check(seed:Int) : Boolean =  {
    val ns = List.fill(4)(Random.nextInt(randIntMax)).map(v => v * randSign)
    val m = 1 + (Math.abs(ns(0)) % tolerantSize)
    val n = 1 + (Math.abs(ns(1)) % tolerantSize)
    seed_check(m, n, seed)
  }
  def seed_check(m:Int, n:Int, seed:Int) : Boolean = {
    Random.setSeed(seed)
    val ns = List.fill(4)(Random.nextInt(randIntMax)).map(v => v * randSign)
    val iStart = ns(2)
    val jStart = ns(3)
    val vals = List.fill(m * n + n)(Random.nextDouble()).map(v => v * randSign * randDoubleMax)
    val a = new Matrix(iStart, (iStart + m -1), jStart, (jStart + n -1), vals)
    val realx = new Matrix(jStart, (jStart + n -1), 0, 0, vals.drop(m * n))
    val b = a * realx
    val result = Util.gauss(a, b)
    val flgSolvable = result._1
    val x = result._6
    val xs = result._7
    val ax = a * x
    val diffB = b.error(ax)
    val diffXs = errorXs(a, xs)
    val freeDim = xs.length
    val state = flgSolvable && isSufficientZero(diffB + diffXs)
    
    if(! state) {
      println("a = ")
      //println(a)
      println("b = ")
      //println(b)
      println("u = ")
      //println(result._4)
      println("ub = ")
      //println(result._5)
      println("P = ", result._2)
      println("Pivots = ", result._3)
      
    }
    
    println("seed, (solvable, m, n, freeDim, diffB, diffXs) = ", seed, (flgSolvable, m, n, freeDim, diffB, diffXs))
    state
  }

  def itr_check(itrMax:Int) {

    var state = false
    var itr = itrMax

    do {
      state = seed_check(Random.nextInt(10000))
      itr = itr - 1
    }while( (itr > 0) && state )
  }

}

