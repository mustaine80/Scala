

sealed trait ExStream[+A] {
  def headOption: Option[A] = this match {
    case ExCons(h, t) => Some(h())
    case _ => None
  }

  def toList: List[A] = this match {
    case ExCons(h, t) => List(h()) ++ t().toList
    case _ => List.empty
  }

  def take(n: Int): ExStream[A] = this match {
    case ExCons(h, t) =>
      if (n > 1) ExStream.cons(h(), t().take(n - 1))
      else ExStream.cons(h(), ExEmpty)

    case _ => ExEmpty
  }

  def drop(n: Int): ExStream[A] = this match {
    case ExCons(h, t) =>
      if (n > 1) t().drop(n - 1)
      else t()
    case _ => ExEmpty
  }

  def takeWhile(p: A => Boolean): ExStream[A] = this match {
    case ExCons(h, t) =>
      if (p(h())) ExStream.cons(h(), t().takeWhile(p))
      else ExEmpty
    case _ => ExEmpty
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
    this.foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  //  todo: 왜 ExStream.empty[A] 는 되는데 ExEmpty[A] 는 오류가 나는 건가?
  def takeWhileByFoldRight(p: A => Boolean): ExStream[A] =
    this.foldRight(ExStream.empty[A])((h, t) =>
      if (p(h)) ExStream.cons(h, t)
      else ExEmpty
    )

  def headOptionByFoldRight: Option[A] =
    this.foldRight(None: Option[A])((h, t) => Some(h))

  def mapByFoldRight[B](f: A => B): ExStream[B] =
    this.foldRight(ExStream.empty[B])((h, t) => ExStream.cons[B](f(h),t))

  def filterByFoldRight(p: A => Boolean): ExStream[A] =
    this.foldRight(ExStream.empty[A])((a, b) => {
      if (p(a)) ExStream.cons(a, b)
      else b
    })

  def appendByFoldRight[B >: A](bs: => ExStream[B]): ExStream[B] =
    this.foldRight(bs)((a, b) => ExStream.cons[B](a, b))

  def flatMapByFoldRight[B](f: A => B): ExStream[B] =
    this.foldRight(ExStream.empty[B])((a, b) => ExStream(f(a)).appendByFoldRight(b))
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


object FP_Chap5 extends App
{
  def expensive(i: Int) = { println("It's so expensive."); i}
  val bar = ExStream(1, 2, 3, 4, 5)

  val x = ExCons(() => expensive(1), () => bar)
  val h1 = x.headOption
  val h2 = x.headOption

  val lazyX = ExStream.cons(expensive(1), bar)
  val l1 = lazyX.headOption

  println("stream list show. " + bar.toList)
  println("stream take 2. " + bar.take(2).toList)
  println("stream drop 2. " + bar.drop(2).toList)
  println("stream takeWhile n < 3. " + bar.takeWhile(_ < 3).toList )
  println("stream exist 3. " + bar.exists(_ == 3))
  println("stream exist 3. " + bar.existsByFoldRight(_ == 3))
  println("stream forAll less than 6. " + bar.forAll(_  < 6))
  println("stream takeWhileByFoldRight " + bar.takeWhileByFoldRight(_ < 4).toList)
  println("stream headOptionByFoldRight " + bar.headOptionByFoldRight.getOrElse(0))
  println("stream mapByFoldRight " + bar.mapByFoldRight(_*2).toList)
  println("stream filterByFoldRight " + bar.filterByFoldRight(_ > 2).toList)
  println("stream appendByFoldRight " + bar.appendByFoldRight(ExStream(1.1, 2.1, 3.1, 4.1, 5.1)).toList)
  println("stream flatMapByFoldRight " + bar.flatMapByFoldRight(_ + 3).toList)
}