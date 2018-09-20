package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  } 
}


object Chapter06 extends App {
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  val (n2, rng3) = rng2.nextInt
  
  println("n1 : " + n1)
  println("n2 : " + n2)
  
  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
   
    if(n1 == Int.MinValue || n1 < 0 || n1 > Int.MaxValue)
      nonNegativeInt(rng2)
    else
      (n1, rng2)    
  }
  
  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng2) = nonNegativeInt(rng)
    
    (n1/Int.MaxValue.toDouble, rng2)
  }
  
  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng2) = nonNegativeInt(rng)
    val (d1, rng3) = double(rng2)
    
    ((n1, d1), rng3)
  }
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (n1, rng3) = nonNegativeInt(rng2)
    
    ((d1, n1), rng3)
  }
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    
    ((d1, d2, d3), rng4)
  }
  
  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, l: List[Int], r: RNG) : (List[Int], RNG) = {
      val (n1, rng2) = nonNegativeInt(r)
      if(count == 1) {        
        (l:::List(n1), rng2)
      } else {        
        go(count - 1, l:::List(n1), rng2)
      }
    }
    
    go(count, List(), rng)
  }
  
  println("6.1) nonNegativeInt : " + nonNegativeInt(rng)._1)
  println("6.2) double : " + double(rng)._1)
  println("6.3) intDouble : " + intDouble(rng)._1._1 + ", " + intDouble(rng)._1._2)
  println("6.3) doubleInt : " + doubleInt(rng)._1._1 + ", " + doubleInt(rng)._1._2)
  println("6.3) double3 : " + double3(rng)._1._1 + ", " + double3(rng)._1._2 + ", " + double3(rng)._1._3)
  println("6.4) ints : " + ints(4)(rng)._1)
  
  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = _.nextInt
  
  def unit[A](a: A): Rand[A] = 
    rng => (a, rng)
    
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
    
  def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i - i % 2)
    
  println("nonNegativeEven : " + nonNegativeEven(rng)._1)
  
  // 6.5
  def doubleUsingMap: Rand[Double] = {
    map(nonNegativeInt)(i => i/Int.MaxValue.toDouble)
  }
  
  println("6.5) double(map) : " + doubleUsingMap(rng)._1)
}