trait Similarity {
  def isSimilar(x: Any) : Boolean
  def isNotSimilar(x: Any) : Boolean = !isSimilar(x)
}

class Point(xc: Int, yc: Int) extends Similarity {
  var x: Int = xc
  var y: Int = yc
  def isSimilar(obj: Any) = {
    obj.isInstanceOf[Point] && obj.asInstanceOf[Point].x == x
  }
}

object TraitTest extends App {
  val p1 = new Point(2, 3)
  val p2 = new Point(2, 4)
  val p3 = new Point(3, 3)
  
  println(p1.isNotSimilar(p2))
  println(p1.isNotSimilar(p3))
  println(p1.isNotSimilar(2))
}


abstract class AbsIterator {
  type T
  def hasNext: Boolean
  def next: T
}

trait RichIterator extends AbsIterator {
  def foreach(f: T=>Unit) { while(hasNext) f(next) }
}

class StringIterator(s: String) extends AbsIterator with RichIterator {
  type T = Char
  private var i = 0
  def hasNext = i < s.length
  def next = { 
    val ch = s.charAt(i)
    i += 1
    ch
  }
}

object StringIteratorTest {
  def main(args: Array[String]) {
    
    val s: String = "This is the test string!!!"
    
    class Iter extends StringIterator(s)// with RichIterator
    val iter = new Iter
    iter foreach print
    println
    
    val iter2 = new StringIterator(s)
    iter2 foreach print
    println
  }
}
