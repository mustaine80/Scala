import scala.math._

object Main extends App {
  val num = 3.14
  val fun = ceil _
  
  println(fun(num))
  
  println( Array(3.14, 1.42, 2.0).map(fun).mkString(", ") )
  
  // anonymous function (lambda)
  val triple = (x: Double) => 3 * x
  def defTriple = (x: Double) => 3 * x
  
  println( Array(3.14, 1.42, 2.0).map(triple).mkString(", ") )
  
  // high order function
  def valueAtOneQuarter(f: (Double) => (Double)) = f(0.25)
  
  println( "valAtOneQuarter(ceil _) : " + valueAtOneQuarter(ceil _) )
  println( "valueAtOneQuarter(sqrt _): " + valueAtOneQuarter(sqrt _) )
  
  // high order function : function return
  def mulBy(factor: Double) = (x: Double) => factor * x
  
  val quintuple = mulBy(5)
  println( "quintuple(20): " + quintuple(20) )
  
  
  // type inference
  println( Array(3.14, 1.42, 2.0).map(3 * _).mkString(", ") )
  
  
  // high order function examples
  (1 to 9).map("*" * _).foreach(println _)
  (1 to 9).map((d: Int) => " " * ((9-d).asInstanceOf[Int]) + "*" * d).foreach(println _)
  (1 to 9).map((d: Int) => " " * (9 - d) + "*" * d).foreach(println _)
  
  
  // Currying
  def mul(x: Int, y: Int) = x * y
//  def mulOneAtATime(x: Int) = (y: Int) => x * y
  def mulOneAtATime(x: Int)(y: Int) = x * y
  
  println( "mulOneAtATime(6)(7): " + mulOneAtATime(6)(7) )
  
  def mulBy2 = mulOneAtATime(2)(_)
  def mulByTwo(x: Int) = mulOneAtATime(2)(x)
  
  println( "mulBy2(7): " + mulBy2(7) )
  println( "mulByTwo(7): " + mulByTwo(7) )
  
  val a = Array("Hello", "World")
  val b = Array("hello", "world")
  println("Corrensponds: " + a.corresponds(b)(_.equalsIgnoreCase(_)) )
  
  
  // Control abstraction
  def runInThread(block: () => Unit) {
    new Thread {
      override def run() { block() }
    }.start()
  }
  
  runInThread { () => println("Hi"); Thread.sleep(1000); println("Bye") }
  
  def runInThread2(block: => Unit) {
    new Thread {
      override def run() { block }
    }.start()
  }
  
  runInThread2 { println("Hi"); Thread.sleep(1000); println("Bye") }
  
  
  def until(condition: => Boolean) (block: => Unit) {
    if(!condition) {
      block
      until(condition)(block)
    }
  }
  
  var x = 10
  until (x == 0) {
    x -= 1
    println(x)
  }
}
