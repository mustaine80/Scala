

sealed trait ExStream[+A] {
  def headOption: Option[A] = this match {
    case ExCons(h, t) => Some(h())
    case _ => None
  }

  //  5.1
  def toList: List[A] = this match {
    case ExEmpty => List.empty
    case ExCons(h, t) => List(h()) ++ t().toList
  }

  //  5.2
  def take(n: Int): ExStream[A] = this match {
    case ExEmpty => ExEmpty
    case ExCons(h, t) =>
      if (n > 1) ExStream.cons(h(), t().take(n - 1))
      else ExStream.cons(h(), ExEmpty)
  }

  def drop(n: Int): ExStream[A] = this match {
    case ExEmpty => ExEmpty
    case ExCons(h, t) =>
      if (n > 1) t().drop(n - 1)
      else t()
  }

  //  5.3
  def takeWhile(p: A => Boolean): ExStream[A] = this match {
    case ExEmpty => ExEmpty
    case ExCons(h, t) =>
      if (p(h())) ExStream.cons(h(), t().takeWhile(p))
      else ExEmpty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case ExCons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case ExCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsByFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  //  5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //  5.5
  //  ExStream.empty 가 기반 형식을 돌려주는게 적합하다. (FP in Scala 88p 참고)
  def takeWhileByFoldRight(p: A => Boolean): ExStream[A] =
    foldRight(ExStream.empty[A])((h, t) =>
      if (p(h)) ExStream.cons(h, t)
      else ExEmpty
    )

  //  5.6
  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  //  5.7
  def mapByFoldRight[B](f: A => B): ExStream[B] =
    foldRight(ExStream.empty[B])((h, t) => ExStream.cons[B](f(h),t))

  def filterByFoldRight(p: A => Boolean): ExStream[A] =
    foldRight(ExStream.empty[A])((a, b) => {
      if (p(a)) ExStream.cons(a, b)
      else b
    })

  def appendByFoldRight[B >: A](bs: => ExStream[B]): ExStream[B] =
    foldRight(bs)((a, b) => ExStream.cons[B](a, b))

  def flatMapByFoldRight[B](f: A => B): ExStream[B] =
    foldRight(ExStream.empty[B])((a, b) => ExStream(f(a)).appendByFoldRight(b))

  def filter(p: A => Boolean): ExStream[A] = this match {
    case ExEmpty => ExEmpty
    case ExCons(h, t) =>
      if (p(h())) ExStream.cons(h(), t().filter(p))
      else t().filter(p)
  }

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  //  5.8
  def constant[A](a: A): ExStream[A] = ExStream.cons(a, constant(a))

  //  5.9
  def from(n: Int): ExStream[Int] = ExStream.cons(n, from(n + 1))

  //  5.10
  def fibs(n1: Int, n2: Int): ExStream[Int] = ExStream.cons(n1, fibs(n2, n1 + n2))

  //  5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): ExStream[A] =
    f(z) match {
      case Some((a, s)) => ExStream.cons(a, unfold(s: S)(f))
      case None => ExEmpty
    }

  //  5.12
  def onesUnfold: ExStream[Int] = unfold(1)( x => Some((1, x)))

  def constantUnfold[A](a: A): ExStream[A] = unfold(a)(x => Some((a, x)))

  def fibsUnfold = unfold((0, 1))( x => Some((x._1 + x._2), ((x._2, x._1 + x._2))))

  //  5.13
  def mapUnfold[B](f: A => B): ExStream[B] = unfold(this){
    case ExCons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeUnfold(n : Int): ExStream[A] = unfold((this, n)) {
    case (ExCons(h, t), 1) => Some((h(), (t(), 0)))
    case (ExCons(h, t), n) if (n > 1) => Some((h(), (t(), n-1)))
    case _ => None
  }

  def takeWhileUnfold(f: A => Boolean): ExStream[A] = unfold(this){
    case ExCons(h, t) if (f(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWithUnfold[B, C](bs: ExStream[B])(f: (A, B) => C): ExStream[C] = unfold((this, bs)){
    case (ExCons(h1, t1), ExCons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: ExStream[B]): ExStream[(Option[A], Option[B])] = unfold((this, s2)){
    case (ExCons(h1, t1), ExCons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (ExCons(h1, t1), ExEmpty) => Some((Some(h1()), None), (t1(), ExEmpty))
    case (ExEmpty, ExCons(h2, t2)) => Some((None, Some(h2())), (ExEmpty, t2()))
    case _ => None
  }

  //  5.14
  def startsWith[A](s: ExStream[A]): Boolean = zipWithUnfold(s)((x, y) => (x, y)) forAll(z => z._1 == z._2)

  //  5.15
  //  todo: this => A 가 성립하지 않기 때문에 cons 가 되지 않음.
  def tails: ExStream[ExStream[A]] = unfold(this) {
    case ExCons(h, t) => Some((this, t()))
    case _ => None
  }.appendByFoldRight(ExEmpty)

  def hasSubsequence[A](s: ExStream[A]): Boolean = tails exists (_ startsWith s)

  //  5.16
  def scanRight[B](z: B)(f: (A, => B) => B): ExStream[B] = unfold(tails){
    case ExCons(h, t) => Some((h().foldRight(z)(f), t()))
    case _ => None
  }
}

case object ExEmpty extends ExStream[Nothing]

case class ExCons[+A](h: () => A, t: () => ExStream[A]) extends ExStream[A]


object ExStream {
  def cons[A](hd: => A, tl: => ExStream[A]): ExStream[A] = {
    lazy val head = hd
    lazy val tail = tl

    ExCons(() => head, () => tail)
  }

  def empty[A]: ExStream[A] = ExEmpty

  def apply[A](as: A*): ExStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


object FP_Chap5 extends App {
  def expensive(i: Int) = {
    println("It's so expensive."); i
  }

  val bar = ExStream(1, 2, 3, 4, 5)
  val car = ExStream(0.1, 0.2, 0.3, 0.4, 0.5)
  val dar = ExStream(6, 7, 8, 9)

  val x = ExCons(() => expensive(1), () => bar)
  val h1 = x.headOption
  val h2 = x.headOption

  val lazyX = ExStream.cons(expensive(1), bar)
  val l1 = lazyX.headOption

  val ones: ExStream[Int] = ExStream.cons(1, ones)

  println("stream list show. " + bar.toList)
  println("stream take 2. " + bar.take(2).toList)
  println("stream drop 2. " + bar.drop(2).toList)
  println("stream takeWhile n < 3. " + bar.takeWhile(_ < 3).toList)
  println("stream exist 3. " + bar.exists(_ == 3))
  println("stream exist 3. " + bar.existsByFoldRight(_ == 3))
  println("stream forAll less than 6. " + bar.forAll(_ < 6))
  println("stream takeWhileByFoldRight(_ < 4) " + bar.takeWhileByFoldRight(_ < 4).toList)
  println("stream headOptionByFoldRight " + bar.headOptionByFoldRight.getOrElse(0))
  println("stream mapByFoldRight(_ * 2) " + bar.mapByFoldRight(_ * 2).toList)
  println("stream filterByFoldRight(_ > 2) " + bar.filterByFoldRight(_ > 2).toList)
  println("stream appendByFoldRight " + bar.appendByFoldRight(ExStream(1.1, 2.1, 3.1, 4.1, 5.1)).toList)
  println("stream flatMapByFoldRight(_ + 3) " + bar.flatMapByFoldRight(_ + 3).toList)
  println("stream find(_ > 3) " + bar.find(_ > 3).toList)
  println("stream infinite stream ones.take(5) " + ones.take(5).toList)
  println("stream infinite stream ones.exists(_ % 2 != 0) " + ones.exists(_ % 2 != 0))
  println("stream infinite stream ones.mapByFoldRight(_ + 1).exists(_ % 2 == 0) " + ones.mapByFoldRight(_ + 1).exists(_ % 2 == 0))
  println("stream infinite stream ones.takeWhile(_ == 1).take(10) " + ones.takeWhile(_ == 1).take(10).toList)
  println("stream infinite stream ones.forAll(_ != 1) " + ones.forAll(_ != 1))
  println("stream from(1) take(10) " + ExStream().from(1).take(10).toList)
  println("stream fibs take(10) " + ExStream().fibs(0,1).take(10).toList)
  println("stream infinite stream onesUnfold.take(5) " + ExStream().onesUnfold.take(5).toList)
  println("stream infinite stream fibsUnfold.take(10) " + ExStream().fibsUnfold.take(10).toList)
  println("stream mapUnfold(_ * 2) " + bar.mapUnfold(_ * 2).toList)
  println("stream takeUnfold(2) " + bar.takeUnfold(2).toList)
  println("stream takeWhileUnfold(_ < 4) " + bar.takeWhileUnfold(_ < 4).toList)
  println("stream zipWithUnfold(IntZipDouble , _ + _) " + bar.zipWithUnfold(car)(_ + _).toList)
  println("Stream zipAll " + bar.zipAll(dar).toList)
  println("Stream startsWith " + bar.startsWith(ExStream(1, 2, 3)))
  println("Stream tails " + bar.tails.toList)
  println("scala.collection.immutable.Stream.tails " + Stream(1, 2, 3, 4, 5).tails.toList)
  println("Stream scanRight " + bar.scanRight(0)(_ + _).toList)
}