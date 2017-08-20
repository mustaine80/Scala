object Twice {
  def unapply(x: Int) : Option[Int] = {
    if(x % 2 == 0) Some(x / 2) else None
  }
  
  def unapply(x: Double) : Option[Double] = {
    if( x % 2 == 0) Some(x / 2) else None
  }
  
  def apply(x: Int) = x * 2
}



object UnapplyTest extends App {
  val x = 10
  val y = 11
  
  val z = Twice(x)
  
  val d = 10.0
  
  d match {
    case Twice(n) => println("n is : " + n)
  }
  
  z match {
    case Twice(n) => println("n is : " + n)
    
  }
  
  y match {
    case Twice(n) => println("N IS : " + n)
    case _ => println("Not matched!!")
  }
}

object DomainSplit extends App {
  def unapplySeq(input: String): Option[Seq[String]] = {
    Some(input.split("\\."))
  }
  
  val s = "www.google.co.kr"
  val s2 = "www.google.com"
  
  s match {
    case DomainSplit(a, b, c) => println("3 : " + a + " " + b + " " + c + " ")
    case DomainSplit(a, b, c, d) => println("4: " + a + " " + b + " " + c + " " + d)
  }
  
  s2 match {
    case DomainSplit(a, b, c) => println("3 : " + a + " " + b + " " + c + " ")
    case DomainSplit(a, b, c, d) => println("4: " + a + " " + b + " " + c + " " + d)
  }
}