

trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f:A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  //  6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  //  6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val (a, rng2) = if (fs.isEmpty) (List[A](), rng) else (List(fs.head(rng)._1), fs.head(rng)._2)
    val (b, rng3) = if (fs.tail.isEmpty) (List[A](), rng2) else sequence(fs.tail)(rng2)
    (a ++ b, rng3)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  //  6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  //  6.9
  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}


case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt

    ((i1, i2), rng3)
  }

  //  6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1) = nextInt
    val i2 = if (i1 == Int.MinValue) 0 else if (i1 < 0) -i1 else i1
    (i2, rng1)
  }

  //  6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)
    val d1 = if (i1 == Int.MaxValue) 0.0 else (i1.toDouble / Int.MaxValue)
    (d1, rng1)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  //  6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)
    val (d1, rng2) = double(rng1)
    ((i1, d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, rng1) = double(rng)
    val (i1, rng2) = nonNegativeInt(rng1)
    ((d1, i1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  //  6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(cnt: Int, ls: List[Int], r: RNG): (List[Int], RNG) = {
      if (cnt > 0) { val (iNext, rNext) = r.nextInt; loop(cnt - 1, ls ++ List(iNext), rNext)}
      else (ls, r)
    }

    loop(count, List[Int](), rng)
  }

  //  6.5
  def graceDouble: Rand[Double] =
    map(nonNegativeInt)(x => if (x == Int.MaxValue) 0.0 else (x.toDouble / Int.MaxValue))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def rollDie: Rand[Int] = mapByFlatMap(nonNegativeLessThan(6))(_ + 1)
}


case class State[S, +A](run: S => (A, S)) {

  def unit[A](a: A): State[S, A] = State(s => (a, s))

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
  )

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }
  )

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    }
  )

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(
    s => {
      val (a, s2) = if (fs.isEmpty) (List[A](), s) else (List(fs.head.run(s)._1), fs.head.run(s)._2)
      val (b, s3) = if (fs.tail.isEmpty) (List[A](), s2) else sequence(fs.tail).run(s2)
      (a++b, s3)
    }
  )

  def r1(Coin: Input): State[Machine, (Int, Int)] = State(
    s => ((s.coins + 1, s.candies), Machine(false, s.candies, s.coins + 1))
  )

  def r2(Turn: Input): State[Machine, (Int, Int)] = State(
    s => ((s.coins, s.candies - 1), Machine(true, s.candies - 1, s.coins))
  )

  def r3(in: Input): State[Machine, (Int, Int)] = State(
    s => ((s.coins, s.candies), s)
  )

  def r4(in: Input): State[Machine, (Int, Int)] = State(
    s => ((s.coins, s.candies), s)
  )

  def setRule(input: Input): State[Machine, (Int, Int)] = State( s => {
    if (s.candies <= 0) r4(input).run(s)
    else if (s.locked && input == Coin) r1(input).run(s)
    else if (!s.locked && input == Turn) r2(input).run(s)
    else r3(input).run(s)
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(s => {
    val rs = for (in <- inputs) yield (setRule(in))
    val sm = sequence(rs.toList).run(s)
    (sm._1.last, sm._2)
  })
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


object FP_Chap6 {
  def main(args:Array[String]): Unit = {
    val rng = SimpleRNG(42)
    import rng._

    println("ints: " + ints(10)(rng)._1)
    println("grace double: " + graceDouble(rng)._1)

    val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)
    println("randIntDouble: " + randIntDouble(rng)._1)

    val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)
    println("randDoubleInt: " + randDoubleInt(rng)._1)

    val fs = List(unit(0), unit(1), unit(2), unit(3))
    println("sequence: " + sequence(fs)(rng)._1)

    println("nonNegativeLessThan: " + nonNegativeLessThan(99)(rng)._1)
    println("rollDie: " + SimpleRNG(5).rollDie(SimpleRNG(5))._1)

    val sm1 = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))
    println("Machine(true, 5, 10) inputs (Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn) = Coins: " + sm1._1._1 + ", Candies: " + sm1._1._2)
  }
}