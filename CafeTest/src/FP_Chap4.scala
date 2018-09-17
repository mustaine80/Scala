

trait ExOption[+A] {
  def exMap[B](f: A => B): ExOption[B] = this match {
    case ExSome(a) => ExSome(f(a))
    case ExNone => ExNone
  }

  def exFlatMap[B](f:A => ExOption[B]): ExOption[B] = exMap(f) exGetOrElse ExNone

  def exFlatten[B](implicit ev: <:<[A, ExOption[B]]): ExOption[B] = ???

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
  def mean(xs: Seq[Double]): ExOption[Double] =
    if (xs.isEmpty) ExNone
    else ExSome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): ExOption[Double] =
    mean(xs).exFlatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def variance2(xs: Seq[Double]): ExOption[Double] =
    if (xs.isEmpty) ExNone
    else {
      val m = xs.foldLeft(0.0)(_ + _) / xs.size
      ExSome(xs.foldLeft(0.0)((acc, x) => acc + math.pow(x - m, 2)) / xs.size)
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

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): ExOption[Double] = {
    val optAge: ExOption[Int] = Try(age.toInt)
    val optTickets: ExOption[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): ExOption[A] = {
    try ExSome(a)
    catch { case e: Exception => ExNone }
  }

  def lift[A, B](f: A => B): ExOption[A] => ExOption[B] = _ exMap f

  def map2[A, B, C](a: ExOption[A], b: ExOption[B])(f: (A, B) => C): ExOption[C] = (a, b) match {
    case (ExNone, _) => ExNone
    case (_, ExNone) => ExNone
    case (x: ExSome[A], y: ExSome[B]) => ExSome(f(x.get, y.get))
  }

  def sequence[A](a: List[ExOption[A]]): ExOption[List[A]] = a match {
    case h :: t => h exFlatMap (hh => sequence(t) exMap (hh :: _))
    case _ => ExSome(List[A]())
  }

  def parseInts(a: List[String]): ExOption[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => ExOption[B]): ExOption[List[B]] =
    sequence(a map f)

  def traverse2[A, B](a: List[A])(f: A => ExOption[B]): ExOption[List[B]] = a match {
    case Nil => ExSome(Nil)
    case h::t => map2(f(h), traverse2(t)(f))(_ :: _)
  }

}


sealed trait ExEither[+E, +A] {
  def eitherMap[B](f: A => B): ExEither[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
}

case class Left[+E](value: E) extends ExEither[E, Nothing]
case class Right[+A](value: A) extends ExEither[Nothing, A]

//  Todo. 레이더 탐지 모델 구현을 FP 답게 개선해 보자.
case class Track(id: Int, pos: Double, vel: Double)

trait SurveillanceModel[+T] {
  def checkAvail(t: T): Boolean = true

  def checkRange(t: T): Boolean = true

  def checkDetect(t: T): Boolean = true

  def updateTrack(t: T): Track = t
}

case class RM(scanPeriod: Double, scanRegion: Double, ts: Seq[Track]) extends SurveillanceModel[Track] {
  def tracking = ts.filter(checkAvail(_)).filter(checkRange(_)).filter(checkDetect(_)).map(updateTrack(_))
}

case class ECM(scanPeriod: Double, scanRegion: Double, ts: Seq[Track]) extends SurveillanceModel[Track] {
  def ecmDetect(t: Track): ExOption[Track] = {
    if (t.id % 2 == 0) ExNone
    else ExSome(t)
  }

  //  Option 문맥에서 기존 함수 재활용
  def tracking = ts.map(ecmDetect(_).exFilter(checkAvail(_)).exFilter(checkRange(_)).exFilter(checkDetect(_)).
                        exMap(updateTrack(_)).exGetOrElse(new Exception("ECM Tracking fail!")))
}


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
  println("lists variance2 => " + Foo.variance2(xs))

  println("Joe's department : " + Bar.joeDepartment)
  println("Joe's default dept : " + Bar.dept)

  println("insurance rate : " + Bar.parseInsuranceRateQuote("18", "20"))

  val failLists = List("1", "2", "3", "foo", "5")
  println("StringToInt must fail. => " + Bar.parseInts(failLists) )
  println("lists traverse => " + Bar.traverse(s)(ExSome(_)))

  println("Either map => " + s.map(Right(_).eitherMap(_ + 1)))

  val ts = Seq(Track(1, 1.0, 10.0), Track(2, 2.2, 12.2), Track(3, 3.3, 13.3))
  val rm = RM(1.5f, 3.14f * 2, ts)
  rm.tracking foreach(println(_))

  val ecm = ECM(3.0f, 3.14f, ts)
  ecm.tracking foreach((println(_)))
}