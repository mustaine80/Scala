trait Buffer {
  type T
  val element: T
}

abstract class SeqBuffer extends Buffer {
  type U
  type T <: Seq[U]
  def length = element.length
}

abstract class IntSeqBuffer extends SeqBuffer {
  type U = Int
}

object AbstractTypeTest1 extends App {
  def newIntSeqBuf(elem1: Int, elem2: Int): IntSeqBuffer = { 
    new IntSeqBuffer {
      type T = List[U]
      val element = List(elem1, elem2)
    }
  }
  
  val buf = newIntSeqBuf(7, 8)
  
  println("length = " + buf.length)
  println("content = " + buf.element)

}

abstract class Buffer2[+T] {
  val element: T
}

abstract class SeqBuffer2[U, +T <: Seq[U]] extends Buffer2[T] {
  def length = element.length
}

object AbstractTypeTest2 extends App {
  def newIntSeqBuf(e1: Int, e2: Int) : SeqBuffer2[Int, Seq[Int]] = {
    new SeqBuffer2[Int, List[Int]] {
      val element = List(e1, e2)
    }
  }
  
  val buf = newIntSeqBuf(7, 8)
  
  println("length = " + buf.length)
  println("content = " + buf.element)
}

object TargetTest2 extends App {
  def loop(body: => Unit): LoopUnlessCond =
    new LoopUnlessCond(body)
  
  protected class LoopUnlessCond(body: => Unit) {
    def unless(cond: => Boolean) {
      body
      if (!cond) unless(cond)
    }
  }
  
  var i = 10
  
  loop {
    println("i = " + i)
    i -= 1
  } unless (i == 0)
}
