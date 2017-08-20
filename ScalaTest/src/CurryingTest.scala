object CurryingTest extends App {
  def multiply(x: Int)(y: Int) = (x * y)
  def multiply2 = multiply(2)(_)
  def multiply3 = multiply(3)(_)
  
  def multiply6(x: Int) = multiply2(multiply3(x))
  
  val ary = Array(1, 2, 3, 4)
  
  //ary.map(multiply(2)).foreach(println)
  ary.map(multiply2).foreach(println)
  println( ary.map(multiply6).map(_.toString).reduce(_ + ", " + _) )
}