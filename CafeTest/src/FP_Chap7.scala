import java.util.concurrent.TimeUnit

import Par.Par
import sun.invoke.empty.Empty

import scala.concurrent.duration.TimeUnit

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = Par.unit(a.call)(this)
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): Option[A]
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

    override def get(timeout: Long, unit: TimeUnit): Option[A] = {
      val start = System.nanoTime()
      //  some async task run... but, simply sleep for demo
      Thread.sleep(1)
      val stop = System.nanoTime()
      val atime = stop - start

      if (atime > timeout) None
      else Some(get)
    }

    override def isCanceled: Boolean = false

    override def cancel(evenIfRunning: Boolean): Boolean = false
  }


  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    var cache: Option[C] = None

    override def isDone: Boolean = cache.isDefined

    override def isCanceled: Boolean = a.isCanceled || b.isCanceled

    override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    override def get: C = f(a.get, b.get)

    override def get(timeout: Long, unit: TimeUnit): Option[C] =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): Option[C] = cache match {
      case Some(c) => cache

      case None =>
        val start = System.nanoTime()
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime()
        val atime = stop - start
        val br = b.get(timeoutInNanos - atime, TimeUnit.NANOSECONDS)

        val ret = for {
          a <- ar
          b <- br
        } yield f(a, b)

        cache = ret
        ret
    }
  }


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  //  7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  //  7.3
  def map2Fix[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

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

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //  7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  // etc
  def map3[A, B, C, D](pa: Par[A])(pb: Par[B])(pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb)((a, b) => f.curried(a)(b)), pc)((cab, c) => cab(c))
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

  def sum5(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    }
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum5(l), sum5(r))(_ + _)
    }
  }
}


object FP_Chap7 {
  import Foo._

  def main(args:Array[String]): Unit = {
    val is = IndexedSeq(1,2,3,4,5)
    println("div & conq sum (IndexedSeq(0,1,2,3,4,5)): " + sum(is))
    println("parallel sum2 (IndexedSeq(0,1,2,3,4,5)): " + sum2(is))

    val es = new ExecutorService

    println("parallel sum3 (IndexedSeq(0,1,2,3,4,5)): " + sum3(is)(es).get)
    println("parallel sum4 (IndexedSeq(0,1,2,3,4,5)): " + sum4(is)(es).get)

    val ls = List(1,2,3,4,5,6,7,8,9,10)

    println("parMap List(n) -> Par[List(n * 10)]: " + Par.parMap(ls)(_ * 10)(es).get)
    println("parFilter List(n) -> Par[List(evenNum)]: " + Par.parFilter(ls)(_ % 2 == 0)(es).get)

    //  7.3
    println("map2Fix timeout '1 msec' return 'None' : " +
      Par.map2Fix(sum5(is), sum5(is))(_ + _)(es).get(1, TimeUnit.MILLISECONDS))
    println("map2Fix timeout '3 msec' return 'Some' : " +
      Par.map2Fix(sum5(is), sum5(is))(_ + _)(es).get(3, TimeUnit.MILLISECONDS))

    //  7.4 ~ 7.5
    Par.sequence(ls.map(Par.asyncF(_ * 10)))(es).get foreach println

    //  7.6
    Par.parFilter(ls){ x =>
      Thread.sleep(500) //  big time
      x % 2 == 0
    }(es).get foreach println
  }
}