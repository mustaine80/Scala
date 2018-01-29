

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

  def head[A](lists: ExList[A]): Option[A] = lists match {
    case Cons(x, xs) => Some(x)
    case Nil => None
  }

  def tail[A](lists: ExList[A]): ExList[A] = lists match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  def setHead[A](elem: A, lists: ExList[A]): ExList[A] = lists match {
    case Cons(x, xs) => Cons(elem, xs)
    case Nil => Cons(elem, Nil)
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
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))   /// not tail recursion
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

  def map[A, B](as: ExList[A])(f: A => B): ExList[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def listsApplyAddOne(l: ExList[Int]): ExList[Int] = ExList.map(l)(_ + 1)

  def listsIntToString(l: ExList[Int]): ExList[String] = ExList.map(l)("'" + _.toString + "'")

  def filter[A](as: ExList[A])(f: A => Boolean): ExList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => 
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f) 
  }

  def flatMap[A, B](as: ExList[A])(f: A => ExList[B]): ExList[B] = flatten(map(as)(f))

  def zipWith[A, B, C](as: ExList[A], bs: ExList[B])(f: (A, B) => C): ExList[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil 
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubSequence[A](sup: ExList[A], sub: ExList[A]): Boolean = ???  /// skip
}


sealed trait ExTree[+A]
case class Leaf[A](value: A) extends ExTree[A]
case class Branch[A](left: ExTree[A], right: ExTree[A]) extends ExTree[A]


object ExTree {
  def size[A](t: ExTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: ExTree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: ExTree[Int]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: ExTree[A])(f: A => B): ExTree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: ExTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}



object Chap3 extends App {
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

  println("lists map(_ + 1) : " + ExList.listsApplyAddOne(lists))
  println("lists map(Int -> String) : " + ExList.listsIntToString(lists))
  println("lists filter(x % 2 == 0) : " + ExList.filter(lists)(_ % 2 == 0))
  println("lists flatMap(List(i, i)) : " + ExList.flatMap(lists)(x => ExList(x, x)))
  println("lists zipWith(x + y) : " + ExList.zipWith(lists, lists)(_ + _))

  val trees = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
  println("tree's size : " + ExTree.size(trees))
  println("tree's max leaf value : " + ExTree.maximum(trees))
  println("tree's depth : " + ExTree.depth(trees))
  println("tree's map f: x * x => " + ExTree.map(trees)(x => x * x))
  println("tree's fold f: x + x => " + ExTree.fold(trees)(_ + 0)(_ + _))
}