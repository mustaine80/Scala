object Chapter02 extends App {
  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a:Int, b: Int): Int =
      if(n == 1) 0
      else if(n == 2) 1
      else if(n == 3) a + b
      else go(n - 1, b, a + b)
      
    go(n, 0, 1)
  }
  
  println(fib(5))
  
  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = 
      if(n == as.length - 2) ordered(as(n), as(n + 1))
      else if(ordered(as(n), as(n + 1))) loop(n + 1)
      else false
      
    loop(0)
  }
  
  println(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))    // ascending
  println(isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a > b))    // descending
  
  // 2.3
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => f(a, _)
  }
  
  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  
  val cc = curry( (a: Int, b: Short) => (a + b).toString() )
  val uc = uncurry( cc )
  
  println(cc(1)(2))
  println(uc(1, 2))
  
  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
    //g andThen f
    // f compose g
  }
}