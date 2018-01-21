

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else	//	todo: 복수의 결재 수단을 사용할 수 있어야 한다.
      throw new Exception("Can't combine charges to different cards")
  }
}


case class CreditCard(name: String)


case class Coffee(price: Double)


object Cafe {
  /** 커피 주문을 받으면 커피와 요금청구를 반환한다.
    * 커피는 고객에게 전달되어야 하겠지.
    * 요금청구는 결재 방법(현금, 카드)에 따라 잔돈을 거슬러 주거나 카드사에 청구해야 할 것이다.
    * 만약 우리 회사라면 사원증도 결재 방법으로 추가되어야 한다!!!
    */
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee(4.99)

    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc)) /// side effect 가 있을 경우 문제가 된다.
    val (coffees, charges) = purchases.unzip
    println(s"$n 개의 커피값 지불이 ${cc.name} 카드로 완료되었습니다.")

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def buyCoffeesWithCurring(cc: CreditCard)(n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    println(s"$n 개의 커피값 지불이 ${cc.name} 카드로 완료되었습니다.")

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}


sealed trait ExList[+A]
case object Nil extends ExList[Nothing]
case class Cons[+A](head: A, tail: ExList[A]) extends ExList[A]

//  chap3 exercise
object ExList {
  def sum(ints: ExList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: ExList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): ExList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](lists: ExList[A]): ExList[A] = lists match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  def setHead[A](elem: A, lists: ExList[A]): ExList[A] = lists match {
    case Cons(x, xs) => Cons(elem, xs)
    case Nil => Nil
  }

  def drop[A](l: ExList[A], n: Int): ExList[A] = l match {
    case Cons(x, xs) =>
      if (n > 0) drop(xs, n - 1)
      else l

    case Nil => Nil
  }

  def dropWhile[A](l: ExList[A], f: A => Boolean): ExList[A] = l match {
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l

    case Nil => Nil
  }

  def init[A](l: ExList[A]): ExList[A] = l match {
    /// ListBuffer 를 이용하면 상수 시간에 해결되긴 하는데 여기에서는 그런 해법을 원하는 것이 아니다.
    /// reverse 는 뒤집는 시간이 있어서 어차피 상수 시간이 아니다.
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => Nil
  }

  def foldRight[A, B](as: ExList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: ExList[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: ExList[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: ExList[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  def foldLeft[A, B](as: ExList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: ExList[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: ExList[Int]) = foldLeft(ns, 1)(_ * _)

  def length3[A](as: ExList[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  def reverse[A](as: ExList[A]): ExList[A] = foldLeft(as, Nil: ExList[A])((acc, h) => Cons(h, acc))

  def append[A](l: ExList[A], r: ExList[A]): ExList[A] = foldRight(l, r)(Cons(_, _))

  def flatten[A](l: ExList[ExList[A]]): ExList[A] = foldRight(l, Nil: ExList[A])(append)
}


object CafeTest extends App {
  Cafe.buyCoffee(CreditCard("VISA"))
//  Cafe.buyCoffees(CreditCard("America Express"), 10)

  /// 만약 우리 가게는 항상 VISA 카드만 받는다면 이것을 부분함수로 만들면 된다.
  val buyCoffeesWithVISA = Cafe.buyCoffees(CreditCard("VISA"), _: Int)
  buyCoffeesWithVISA(10)

  /// 손님이 5 잔을 주문했는데, 결재는 나갈때 한다고 하면(?) 커링을 사용한다.
  val buyFiveCoffees = Cafe.buyCoffeesWithCurring(_: CreditCard)(5)
  buyFiveCoffees(CreditCard("America Express"))

  /// 아니면 선결재하고 커피는 사람들이 오면 만들어 달라고 할 때에도 커링이 효과적이다.
  val buyCoffeesPrepayment = Cafe.buyCoffeesWithCurring(CreditCard("법인"))(_: Int)
  buyCoffeesPrepayment(20)

  //  chap3 ex
  println("\n---------------------chap3 exercise ------------------------")
  val lists = ExList(1,2,3,4,5)
  println("lists tail : " + ExList.tail(lists))
  println("lists setHead(0) : " + ExList.setHead(0, lists))
  println("lists drop(3) : " + ExList.drop(lists, 3))

  def proc(x: Int): Boolean = x < 2
  println("lists dropWhile(n < 2) : " + ExList.dropWhile(lists, proc))

  println("lists init : " + ExList.init(lists))
  println("lists foldRight with Cons. It's original lists. : " 
    + ExList.foldRight(lists, Nil:ExList[Int])(Cons(_,_)))

  println("lists length : " + ExList.length(lists))

  println("foldLeft sum : " + ExList.sum3(lists))
  println("foldLeft product : " + ExList.product3(lists))
  println("foldLeft length : " + ExList.length3(lists))

  println("lists reverse : " + ExList.reverse(lists))
  println("lists append : " + ExList.append(lists, ExList(6,7,8,9,10)))
  println("lists flatten : " + ExList.flatten(ExList(lists, ExList(6,7), ExList(8,9,10))))
 }
