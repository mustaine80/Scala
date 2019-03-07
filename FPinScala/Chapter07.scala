package chapter07

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic._
import java.util.concurrent._

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = {
    val c = a.call
    
    println("ExecutorService::submit call result: " + c)
    
    
    new Future[A] {
      def get = c
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = c
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }
  }
}
trait Callable[A] { def call: A }
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

sealed trait Future2[A] {
  def apply(k: A => Unit): Unit
}

object Par extends App {
  type Par[A] = ExecutorService => Future[A]
  
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }
  
  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }
  
  
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = { 
    map2(parList, unit(()))((a, _) => a.sorted)
  }
  
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
  }
  
  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(_.sorted)
  }
  
  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    //es => UnitFuture(ps.map( (pa: Par[A]) => run(es)(pa).get ) )    
    //ps.map( (p: Par[A]) => map(p)(List(_)) ).reduce( (p1, p2) => map2(p1, p2)( (l1, l2) => l1 ++ l2 ) )
    ps.map( map(_)(List(_)) ).reduce( map2(_, _)( _ ++ _ ) )
  }
  
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  
  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    /*
    // 이 코드는 병렬 실행이 아님
    val p = asyncF( (l: List[A]) => l.filter(f) )
    p(as)
    */
    // 아래 코드보다 더 좋은 방법이 없을 것인가...?
    // 아래 코드는 마지막에 filter로 한 번 더 걸러내는 방식이라 작업의 중복이 발생함
    val ff = (a: A) => if(f(a)) (a, true) else (a, false)
    val m = as.map(asyncF(ff))
    val seq = sequence(m)
    map(seq)( l => l.filter(_._2).map(_._1) )
  }
  
  //def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(fc: (A, B, C) => D): Par[D] = {
  //  
  //}
  
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
    p(e).get == p2(e).get
  }
  
  def delay[A](fa: => Par[A]): Par[A] = {
    es => fa(es)
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  type Par2[A] = ExecutorService => Future2[A]
  
  def run2[A](es: ExecutorService)(p: Par2[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }
  
  def unit2[A](a: A): Par2[A] = {
    es => new Future2[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
  }
  
  def fork2[A](a: => Par2[A]): Par2[A] = {
    es => new Future2[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }
  }
  
  def eval(es: ExecutorService)(r: => Unit): Unit = {
    es.submit(new Callable[Unit] { def call = r})
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  
  //////////////////////
  // test code
  //////////////////////
  
  // 7.5
  def func1 {
    println("func1")
  }
  def func2 {
    println("func2")
  }
  def func3 {
    println("func3")
  }
  
  val par1 = unit(func1)
  val par2 = unit(func2)
  val par3 = unit(func3)
  val lpar1 = lazyUnit(func1)
  val lpar2 = lazyUnit(func2)
  val lpar3 = lazyUnit(func3)
  val lp = List(par1, par2, par3)
  val llp = List(lpar1, lpar2, lpar3)
  val pl = sequence(lp)
  val lpl = sequence(llp)
  
  val es = new ExecutorService
  run(es)(pl)
  println("List[Par[A]]: " + lp)
  println("Par[List[A]]: " + pl)
  
  run(es)(lpl)
  
  val il = List(10, 20, 30, 40, 50)
  val pi = parMap(il)(_ * 10)
  val r2 = run(es)(pi)
  
  println(r2.get)
  
  // 7.6
  val pf = parFilter(il)(_ % 20 != 0)
  
  val r3 = run(es)(pf)
  
  println(r3.get)
}