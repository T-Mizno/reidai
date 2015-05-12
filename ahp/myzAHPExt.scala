package myzAHP 

// scalac -d classes myzLinear.scala
// scalac -d classes -classpath classes myzAHP.scala
// scalac -d classes -classpath .\classes;c:\local\java-ext-libs\poi-3.9-20121203.jar myzAHPExt.scala

object ExtUtil {

  import myzLinear.Matrix
  import org.apache.poi.hssf.usermodel._
  import java.io.FileOutputStream
  import java.io.FileInputStream
  import java.io.File

  def writeMatrix2Sheet(mat:Matrix, sheet:HSSFSheet) {
    for(i <- 0 until mat.m) {
      val row = sheet.createRow(i)
      for(j <- 0 until mat.n) {
	val cell = row.createCell(j)
	cell.setCellValue(mat.at(i,j))
      }
    }    
  }

  def setMatrixFromSheet(mat:Matrix, sheet:HSSFSheet) {
    for(i <- 0 until mat.m) {
      for(j <- 0 until mat.n) {
	mat.set(i,j, sheet.getRow(i).getCell(j).getNumericCellValue())
      }
    }
  }

  def readExcelFile(filename:String):Either[Exception, DominantAHP] = {
    readExcelFile(new File(filename))
  }
  def readExcelFile(file:File):Either[Exception, DominantAHP] = {
    var iStream:FileInputStream = null
    try {
      iStream = new FileInputStream(file)
      val book = new HSSFWorkbook(iStream)
      val specSheet = book.getSheet("spec")
      val aM = specSheet.getRow(0).getCell(0).getNumericCellValue().toInt
      val aN = specSheet.getRow(0).getCell(1).getNumericCellValue().toInt
      val aDAHP = new DominantAHP(aM, aN)
      val aDNum = specSheet.getRow(0).getCell(2).getNumericCellValue().toInt
      var ds:Set[Int] = Set.empty
      for(i <- 0 until aDNum) {
	ds += specSheet.getRow(1).getCell(i).getNumericCellValue().toInt
      }
      val aNames = new Array[String](aM)
      for(i <- 0 until aM){
	aNames(i) = specSheet.getRow(2).getCell(i).getStringCellValue()
      }
      aDAHP.setAltNames(aNames)
      val cNames = new Array[String](aN)
      for(i <- 0 until aN){
	cNames(i) = specSheet.getRow(3).getCell(i).getStringCellValue()
      }
      aDAHP.setCNames(cNames)
      val spcmSheet = book.getSheet("spcm")
      val aSPCM = aDAHP.spcm
      setMatrixFromSheet(aSPCM, spcmSheet)
      aDAHP.setFromSPCM(aSPCM, ds)
      return Right(aDAHP)
    }
    catch {
//      case es:java.lang.IllegalStateException => Left(es)
//      case ei:java.io.IOException => Left(ei) 
      case e:Exception => {
	e.printStackTrace
	println(e)
	return Left(e)
      }
    }
    finally {
      if(iStream != null) iStream.close()
    }
    Left(new Exception("Cannot read File " + file))
  }

  def writeExcelFile(dahp:DominantAHP, filename:String):Either[Exception, String] = {
    writeExcelFile(dahp, new File(filename))
  }
  def writeExcelFile(dahp:DominantAHP, file:File):Either[Exception, String] = {

    var oStream: FileOutputStream = null
    try {
      var count = 0

      val book = new HSSFWorkbook

      //spec sheet
      val specSheet = book.createSheet("spec")
      val specRow = specSheet.createRow(0)
      val mCell = specRow.createCell(0)
      val nCell = specRow.createCell(1)
      val dCell = specRow.createCell(2)
      mCell.setCellValue(dahp.m)
      nCell.setCellValue(dahp.n)
      dCell.setCellValue(dahp.bs.keySet.size)

      val dRow = specSheet.createRow(1)
      count = 0
      for(d <- dahp.bs.keySet.toList.sorted) {
	val tmpDCell = dRow.createCell(count)
	tmpDCell.setCellValue(d)
	count = count + 1
      }
      
      val altNameRow = specSheet.createRow(2)
      count = 0
      for(i <- 0 until dahp.m) {
	val aCell = altNameRow.createCell(count)
	aCell.setCellValue(dahp.altNames(i))
	count = count + 1
      }
      val crNameRow = specSheet.createRow(3)
      count = 0
      for(j <- 0 until dahp.n) {
	val cCell = crNameRow.createCell(count)
	cCell.setCellValue(dahp.cNames(j))
	count = count + 1
      }

      //spcm
      val spcmSheet = book.createSheet("spcm")
      writeMatrix2Sheet(dahp.spcm, spcmSheet)

      oStream = new FileOutputStream(file, false) // append = false
      book.write(oStream)
      return Right(file.toString)
    }
    catch {
      case e:Exception => return Left(e)
    }
    finally {
      if(oStream != null ) oStream.close
    }
    Left(new Exception("Cannot write to file :" + file))
  }

}
