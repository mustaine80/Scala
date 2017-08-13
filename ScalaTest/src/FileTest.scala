import scala.io.Source
import java.io._
import java.nio.file._

object FileTest extends App {
  val source = Source.fromFile("myfile.txt")
  val lineIterator = source.getLines
  val lines = lineIterator.toArray
  val constr = lines.reduce(_ + " " + _)
  
  //for(l <- lineIterator) println(l)
  //lineIterator.foreach(println)
  lines.map("<< " + _ + " >> ").foreach(println)
  
  println(constr)
  
  val tokens = constr.split("\\s+")
  tokens.foreach(println)
  
  val file = new File("myfile.txt")
  val in = new FileInputStream(file)
  val bytes = new Array[Byte](file.length.toInt)
  in.read(bytes)
  in.close()
  
  println(bytes.map(_.toChar.toString).reduce(_ + "," + _) )
  
  // directory traversal test
  val dir = new File("/Users/yongheonlee/Documents")
  val subdir = subdirs(dir)
//  for(d <- subdir) println(d.toString)
  
  def subdirs(dir: File): Iterator[File] = {
    val children = dir.listFiles.filter(_.isDirectory)
    children.toIterator ++ children.toIterator.flatMap(subdirs _)
  }
  
  // 암묵적 변환 기능을 사용한 함수 -> 인터페이스 변환
  implicit def makeFileVisitor(f: (java.nio.file.Path) => Unit) = new SimpleFileVisitor[java.nio.file.Path] {
    override def visitFile(p: java.nio.file.Path, attrs: attribute.BasicFileAttributes) = {
      f(p)
      FileVisitResult.CONTINUE
    }
  }
  
  // walkFileTree에서 요구하는 FileVisitor 인터페이스 대신에 scala 함수로 대체. 암묵적 변환이 일어남
  java.nio.file.Files.walkFileTree(dir.toPath, (f: java.nio.file.Path) => println(f))
  
}