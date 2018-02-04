

trait ExOption[+A] {
  def exMap[B](f: A => B): ExOption[B] = this match {
    case ExSome(a) => ExSome(f(a))
    case ExNone => ExNone
  }

  def exFlatMap[B](f:A => ExOption[B]): ExOption[B] = exMap(f) exGetOrElse ExNone

  def exGetOrElse[B >: A](default: => B): B = this match {
    case ExSome(a) => a
    case ExNone => default
  }

  def exOrElse[B >: A](ob: => ExOption[B]): ExOption[B] = this match {
    case ExSome(a) => this
    case ExNone => ob
  }

  //  Option return 이 맞는건지 모르겠네...
  def exFilter(f: A => Boolean): ExOption[A] = this match {
    case ExSome(a) => if (f(a)) this else ExNone
    case ExNone => ExNone
  }
}

case class ExSome[+A](get: A) extends ExOption[A]
case object ExNone extends ExOption[Nothing]


object FP_Chap4 extends App {
  val s = List(1,2,3,4,5)
  val oe = List(ExNone, ExSome(1), ExNone)
  def foo(a: Int): ExOption[Int] = if (a %2 == 0) ExSome(a) else ExNone

  //  todo: 실제 Option 구현도 다를게 없는데 그건 왜 List 가 적용이 되는건가?
  //  @inline final def map[B](f: A => B): Option[B] = if (isEmpty) None else Some(f(this.get))
  println("lists map apply (_ + 1) => " + s.map(ExSome(_).exMap(_ + 1)))
  println("lists flatMap apply 'even number' => " + s.map(ExSome(_).exFlatMap(x => foo(x))))
  println("lists getOrElse apply 'even number' => " + s.map(ExSome(_).exFlatMap(x => foo(x)).exGetOrElse(0)))
  println("lists orElse apply None to Some(1) => " + oe.map(_.exOrElse(ExSome(1))))
  println("lists filter apply 'even number' => " + s.map(ExSome(_).exFilter(_ % 2 == 0)))
}