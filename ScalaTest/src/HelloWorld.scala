
class Color(val red:Int, val green:Int, val blue:Int)

case class RGB(r:Int, g:Int, b:Int) extends Color(r, g, b)
case class Red(r:Int) extends Color(r, 0, 0)
case class Green(g:Int) extends Color(0, g, 0)
case class Blue(b:Int) extends Color(0, 0, b)

object HelloWorld {
  def main(args: Array[String]) {
    println("Hello world!")
    
/*    import sys.process._
    "ls -al ." !*/
    
    import sys.process._
    //val result = "ls -al ." #| "grep bin" !!
    val result = "ls -al ." !!
    
    println(result)
    
    
//    (1 to 10).foreach(println(_))
    List.range(1, 10).foreach(println(_))
    
    //println( getPrimeNumbers(100).map(_.toString).reduce(_ + ", " + _) )
    val num = 50000
    println("elapsed time (sequential) : " + getElapsedTime(prime, num, 1000000.0) + " ms")
    println("elapsed time (parallel) : " + getElapsedTime(primePar, num, 1000000.0) + " ms")
    
    
    printColor(Red(10))
    printColor(new Color(200, 100, 150))
    printColor(RGB(100, 102, 103))
  }
  
  def printColor(c:Color) = c match {
    case Red(v) => println("Red: " + v)
    case Green(v) => println("Green: " + v)
    case Blue(v) => println("Blue: " + v)
    case col:Color => {
      print("R: " + col.red + ", ")
      print("G: " + col.green + ", ")
      print("B: " + col.blue)
      print("\n")
    }
    case null => println("invalid color!")
  }
 
  def prime(num : Int) {
    getPrimeNumbers(num)
  }
  
  def primePar(num : Int) {
    getPrimeNumbersPar(num)
  }
  
  def isPrime(num : Int) = {
    val candidate = Math.sqrt(num.asInstanceOf[Double]).asInstanceOf[Int]
    (2 to candidate).filter(num % _ == 0).size == 0
  }
  
  def getPrimeNumbers(num : Int) = {
    (2 to num).filter(isPrime(_)).map(_.toString)
    //println( (2 to num).filter(isPrime(_)).map(_.toString).reduce(_ + ", " + _) )
  }
  
  def getPrimeNumbersPar(num : Int) = {
    (2 to num).par.filter(isPrime(_)).map(_.toString)
    //println( (2 to num).par.filter(isPrime(_)).map(_.toString).reduce(_ + ", " + _) )
  }
  
  
  def getElapsedTime(func : (Int)=>Unit, num:Int, factor : Double) : Double = {
    val from = System.nanoTime();
    func(num)
    val to = System.nanoTime();
    
    (to - from) / factor
  }
}


