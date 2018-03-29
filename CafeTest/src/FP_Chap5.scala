

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
}