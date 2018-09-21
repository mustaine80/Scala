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
  
  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
  
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)((_, _))
    
  val randIntDouble: Rand[(Int, Double)] = 
    both(nonNegativeInt, double)
  
  val randDoubleInt: Rand[(Double, Int)] = 
    both(double, nonNegativeInt)
    
  println("6.6) randIntDouble : " + randIntDouble(rng)._1._1 + ", " + randIntDouble(rng)._1._2)
  println("6.6) randDoubleInt : " + randDoubleInt(rng)._1._1 + ", " + randDoubleInt(rng)._1._2)
  
  // 6.7 (difficult)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def go(lr: List[Rand[A]], la: List[A], r: RNG): (List[A], RNG) = {
      if(lr.length > 1) {
        val (a, rng2) = lr.head(r)
        go(lr.tail, la ::: List(a), rng2)
      } else if(lr.length == 1) {
        val (a, rng2) = lr.head(r)
        (la ::: List(a), rng2)
      } else {
        (la, r)
      }
    }
    
    rng => {      
      go(fs, List(), rng)
    }
  }
  
  val l: List[Rand[Int]] = List(nonNegativeInt, int)
  val seq = sequence(l)
  println("6.7) sequence : " + seq(rng))
  
  def intsUsingSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    val fs = List.fill(count)(nonNegativeInt _)
    
    sequence(fs)(rng)
  }
  
  println("6.7) intsUsingSequence : " + intsUsingSequence(4)(rng)._1)
  
  def nonNegativeLessThan1(n: Int): Rand[Int] = 
    map(nonNegativeInt) { _ % n }
  
  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if(i + (n-1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan2(n)(rng2)
  }
  
  println("nonNegativeLessThan : " + nonNegativeLessThan2(100)(rng)._1)
  
  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val ff = f(rng)
      g(ff._1)(ff._2)      
    }
  }
  
  val fm = flatMap(nonNegativeInt _)(a => { rng => (a + 1, rng) })
  println("6.8) flatMap : " + fm(rng)._1)
  
  def nonNegativeLessThanUsingFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt _)(i => { rng =>
      val mod = i % n
      
      if(i + (n-1) - mod >= 0)
        (mod, rng)
      else {
        nonNegativeLessThanUsingFlatMap(n)(rng)
      }
    })
  }
  
  println("6.8) nonNegativeLessThan(flatMap) : " + nonNegativeLessThanUsingFlatMap(100)(rng)._1)
  
  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => { rng => 
      (f(a), rng)
    })
  }
  
  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C ): Rand[C] = {
    flatMap(ra)(a => { rng => 
      flatMap(rb)(b => { rng2 => 
        (f(a, b), rng2)
      })(rng)
    })
  }
  
  def bothFlatMap[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2UsingFlatMap(ra, rb)((_, _))
    
  val randIntDoubleFlatMap: Rand[(Int, Double)] = 
    bothFlatMap(nonNegativeInt, double)
  
  val randDoubleIntFlatMap: Rand[(Double, Int)] = 
    bothFlatMap(double, nonNegativeInt)
    
  println("6.8) randIntDouble(map2 flatMap) : " + randIntDoubleFlatMap(rng)._1._1 + ", " + randIntDoubleFlatMap(rng)._1._2)
  println("6.8) randDoubleInt(map2 flatMap) : " + randDoubleIntFlatMap(rng)._1._1 + ", " + randDoubleIntFlatMap(rng)._1._2)
}