package parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

sealed trait Future[A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown
      println("work complete. callback")
    }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      override def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      override def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //  7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  //  7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(List[A]())
      case h::t => map2(map(h)(List[A](_)), sequence(t))(_ ++ _)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))
}


object NBPar {
  def main(args: Array[String]): Unit = {
    import Par._

    val es = Executors.newFixedThreadPool(2)
    println("run unit(1) => " + run(es)(unit(1)))
    println("run fork(unit(1)) => " + run(es)(fork(unit(1))))

    //  todo  3k~100k tasks cause an error
    val start = System.currentTimeMillis()
    val p = parMap(List.range(1, 3000))(math.sqrt(_))
    run(es)(p).foreach(println(_))
    val end = System.currentTimeMillis()
    val atime = end - start
    println(s"parallel task takes ${atime} ms")   //  266 ms

    val start2 = System.currentTimeMillis()
    List.range(1, 3000).map(math.sqrt(_)) foreach println _
    val end2 = System.currentTimeMillis()
    val atime2 = end2 - start2
    println(s"single task takes ${atime2} ms")    //  26 ms... - -;;

    es.shutdown
  }
}
