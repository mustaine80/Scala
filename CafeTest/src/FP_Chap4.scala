

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

  def exFilter(f: A => Boolean): ExOption[A] = this match {
    case ExSome(a) => if (f(a)) this else ExNone
    case ExNone => ExNone
  }
}

case class ExSome[+A](get: A) extends ExOption[A]
case object ExNone extends ExOption[Nothing]

object Foo {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def variance2(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else {
      val m = xs.foldLeft(0.0)(_ + _) / xs.size
      Some(xs.foldLeft(0.0)((acc, x) => acc + math.pow(x - m, 2)) / xs.size)
    }
  }
}

case class Employee(name: String, department: String)

object Bar {
  def lookupByName(name: String): Option[Employee] = {
    val employeeDB = List(Employee("aoa", "HR"), Employee("twice", "JYP"), Employee("Joe", "Accounting"))
    employeeDB.find(name == _.name)
  }

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
  val dept: String =
    lookupByName("Joe").
      map(_.department).
      filter(_ != "Accounting").
      getOrElse("Default Dept")

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    (age + numberOfSpeedingTickets) / 12.0
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    if (a == None || b == None) None
    else Some(f(a.get, b.get))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    case _ => Some(List[A]())
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    sequence(a map f)
}


sealed trait ExEither[+E, +A] {
  def eitherMap[B](f: A => B): ExEither[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
}

case class Left[+E](value: E) extends ExEither[E, Nothing]
case class Right[+A](value: A) extends ExEither[Nothing, A]



object FP_Chap4 extends App {
  val s = List(1,2,3,4,5)
  val oe = List(ExNone, ExSome(1), ExNone)
  def foo(a: Int): ExOption[Int] = if (a %2 == 0) ExSome(a) else ExNone

  println("lists map apply (_ + 1) => " + s.map(ExSome(_).exMap(_ + 1)))
  println("lists flatMap apply 'even number' => " + s.map(ExSome(_).exFlatMap(x => foo(x))))
  println("lists getOrElse apply 'even number' => " + s.map(ExSome(_).exFlatMap(x => foo(x)).exGetOrElse(0)))
  println("lists orElse apply None to Some(1) => " + oe.map(_.exOrElse(ExSome(1))))
  println("lists filter apply 'even number' => " + s.map(ExSome(_).exFilter(_ % 2 == 0)))

  val xs = List(0.0, 2.0, 4.0, 6.0, 8.0)
  println("lists variance => " + Foo.variance(xs))

  println("Joe's department : " + Bar.joeDepartment)
  println("Joe's default dept : " + Bar.dept)

  println("insurance rate : " + Bar.parseInsuranceRateQuote("18", "20"))

  val failLists = List("1", "2", "3", "foo", "5")
  println("StringToInt must fail. => " + Bar.parseInts(failLists) )
  println("lists traverse => " + Bar.traverse(s)(Some(_)))

  println("Either map => " + s.map(Right(_).eitherMap(_ + 1)))
}