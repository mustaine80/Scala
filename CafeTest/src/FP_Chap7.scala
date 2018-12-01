import Par.Par

import scala.concurrent.duration.TimeUnit

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = Par.unit(a.call)(this)
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCanceled: Boolean
}


//  7.2
object Par
{
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCanceled: Boolean = false

    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  //  7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) //  todo 7.3: respect timeouts
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  //  todo 7.3
  def map2Fix[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  //  7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  //  7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(List[A]())
      case h::t => map2(map(h)(List[A](_)), sequence(t))(_ ++ _)
    }

  //  7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    sequence(as.filter(f).map(unit(_)))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
}


object Foo {
  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption getOrElse 0
    }
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }

  def sum2(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = Par.unit(sum2(l))
      val sumR: Par[Int] = Par.unit(sum2(r))
      Par.run(new ExecutorService)(sumL).get + Par.run(new ExecutorService)(sumR).get
    }
  }

  def sum3(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum3(l), sum3(r))(_ + _)
    }
  }

  def sum4(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum4(l)), Par.fork(sum4(r)))(_ + _)
    }
  }
}


object FP_Chap7 {
  import Foo._

  def main(args:Array[String]): Unit = {
    println("div & conq sum (IndexedSeq(0,1,2,3,4,5)): " + sum(IndexedSeq(1,2,3,4,5)))
    println("parallel sum2 (IndexedSeq(0,1,2,3,4,5)): " + sum2(IndexedSeq(1,2,3,4,5)))

    val es = new ExecutorService

    println("parallel sum3 (IndexedSeq(0,1,2,3,4,5)): " + Par.run(es)(sum3(IndexedSeq(1,2,3,4,5))).get)
    println("parallel sum4 (IndexedSeq(0,1,2,3,4,5)): " + Par.run(es)(sum4(IndexedSeq(1,2,3,4,5))).get)

    val ls = List(1,2,3,4,5,6,7,8,9,10)

    println("parMap List(n) -> Par[List(n * 10)]: " + Par.parMap(ls)(_ * 10)(es).get)
    println("parFilter List(n) -> Par[List(evenNum)]: " + Par.parFilter(ls)(_ % 2 == 0)(es).get)
  }
}