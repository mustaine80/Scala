import java.util.concurrent._

import Par.Par

//  7.2
object Par
{
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = true

    override def isCancelled: Boolean = true

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
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

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

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

  // todo map4? map5?
  def map3[A, B, C, D](pa: Par[A])(pb: Par[B])(pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb)((a, b) => f.curried(a)(b)), pc)((cab, c) => cab(c))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  //  7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es) //  todo 예외 처리 없음

  def choiceWithChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val choices = List[Par[A]](t, f)
    val n = map(cond)(x => if (x) 1 else 0)
    choiceN(n)(choices)
  }

  //  7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es) //  todo 예외 처리 없음

  //  7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get)(es)

  def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(x => if (x) t; else f)

  def choiceNWithChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(x => choices(x))

  //  7.14
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get.apply(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)
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
//      Par.run(new ExecutorService)(sumL).get + Par.run(new ExecutorService)(sumR).get
      Par.run(Executors.newFixedThreadPool(1))(sumL).get
        + Par.run(Executors.newFixedThreadPool(1))(sumL).get
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

    val es = Executors.newScheduledThreadPool(4)

    println("parallel sum3 (IndexedSeq(0,1,2,3,4,5)): " + sum3(is)(es).get)
    println("parallel sum4 (IndexedSeq(0,1,2,3,4,5)): " + sum4(is)(es).get)

    val ls = List(1,2,3,4,5,6,7,8,9,10)

    println("parMap List(n) -> Par[List(n * 10)]: " + Par.parMap(ls)(_ * 10)(es).get)
    println("parFilter List(n) -> Par[List(evenNum)]: " + Par.parFilter(ls)(_ % 2 == 0)(es).get)

    //  7.4 ~ 7.5
    Par.sequence(ls.map(Par.asyncF(_ * 10)))(es).get foreach println

    //  7.6
    Par.parFilter(ls){ x =>
      Thread.sleep(500) //  big time
      x % 2 == 0
    }(es).get foreach println

    println("map(unit(1))(_ + 1) == unit(2) ? " +
      Par.equal(es)(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))

    //  7.7
    //  map(y)(id) == y
    //  map(map(y)(id))(f) == map(y)(f)
    //  map(map(y)(id))(f) == map(y)(f compose id)
    //  map(map(y)(g))(f) == map(y)(f compose g)

    //  7.8
    val a = Par.lazyUnit(42 + 1)
    val es2 = Executors.newFixedThreadPool(1)
//    println(Par.equal(S)(a, Par.fork(a)))   //  deadlock

    //  7.9
    //  fork(fork(fork(...))

    println("def delay no more deadlock. unit(42+1) == delay(unit(42+1)) ? " +
      Par.equal(es2)(a, Par.delay(a)))

    es2.shutdown()

    //  7.11
    val pList = List(Par.unit("zero"), Par.unit("one"), Par.unit("two"), Par.unit("three"))
    println("2nd pick up from choiceN => " + Par.run(es)(Par.choiceN(Par.unit(2))(pList)).get())

    //  7.12
    val pMap = Map(1 -> Par.unit("one"), 2 -> Par.unit("two"), 3 -> Par.unit("three"))
    println("key[3]'s value from choiceMap => " + Par.run(es)(Par.choiceMap(Par.unit(3))(pMap)).get())

    //  7.13
    println("choose word 'scenario' and return length => "
      + Par.run(es)(Par.chooser(Par.unit("scenario"))(x => Par.unit(x.length))).get())

    //  7.14
    println("join unit(unit(10)) => " + Par.run(es)(Par.join(Par.unit(Par.unit(10)))).get())

    es.shutdown()
  }
}