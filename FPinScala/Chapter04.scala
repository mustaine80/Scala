case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case None => None
    }
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(v) => f(v)
      case None => None
    }
  }
  
  def flatMapWithoutPatternMatch[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }
  
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case None => default
    }
  }
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(v) => this
      case None => ob
    }
  }
  
  def orElseWithoutPatternMatch[B >: A](ob: => Option[B]): Option[B] = {
    this.map( o => Some(o) ).getOrElse(ob)
  }
  
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) => if(f(v)) this else None
      case None => None
    }
  }
  
  def filterWithoutPatternMatch(f: A => Boolean): Option[A] = {
    if(this.map(f).getOrElse(false)) this else None
  }
}

object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }
  
  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap( aa =>
      b.map(bb =>
        f(aa, bb) ) )
  }
  
  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    Try(List.flatMap(as)(o => o match {
      case Some(v) => List(v)
      case None => throw new Exception
    }) )
  }
  
  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(List.map(as)(f))
  }
  
  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    Try(List.flatMap(as)(a => f(a) match {
        case Some(v) => List(v)
        case None => throw new Exception
      })
    )
  }
  
  def sequenceUsingTraverse[A](as: List[Option[A]]): Option[List[A]] = {
    traverse2(as)(a => a)
  }
}

sealed trait Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(r) => Right(f(r))
      case Left(l) => Left(l):Either[E, B]
    }
  }
  
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(r) => f(r)
      case Left(l) => Left(l):Either[EE, B]
    }
  }
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(r) => this
      case Left(l) => b
    }
  }
  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(aa =>
      b.map(bb =>
        f(aa,bb) )
    )  
  }
  
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }
  
  // 4.7
  /*
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    val result: Either[Exception, List[A]] = Try( List.flatMap(es)(e => e match {
      case Right(r) => List(r)
      case Left(l) => throw new Exception("ERROR!")
    }) )
    
    result
  }*/
  /*
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    
  }*/
}

object Chapter04 extends App {
  val o1s = Some(1)
  val o2s = o1s.map(_ * 10)
  val o2n: Option[Int] = None
  val o3s = Some(100)
  
  // 4.1
  println("map(Some): " + o2s)
  println("map(None): " + o2n)
  
  println("flatMap(Some): " + o1s.flatMap(a => Some(a*2)))
  println("flatMap(None): " + o1s.flatMap(a => None))
  println("flatMapWOPM(Some): " + o1s.flatMapWithoutPatternMatch(a => Some(a*2)))
  println("flatMapWOPM(None): " + o1s.flatMapWithoutPatternMatch(a => None))
  
  println("getOrElse(Some): " + o2s.getOrElse(0))
  println("getOrElse(None): " + o2n.getOrElse(0))
  
  println("orElse(Some): " + o2s.orElse(o3s))  // o2s
  println("orElse(None): " + o2n.orElse(o3s))  // o3s
  println("orElseWOPM(Some): " + o2s.orElseWithoutPatternMatch(o3s))
  println("orElseWOPM(None): " + o2n.orElseWithoutPatternMatch(o3s))
  
  println("filter(Some): " + o2s.filter(_ % 2 == 0))  // true
  println("filter(None): " + o2s.filter(_ % 2 == 1))  // false
  println("filterWOPM(Some): " + o2s.filterWithoutPatternMatch(_ % 2 == 0))  // true
  println("filterWOPM(None): " + o2s.filterWithoutPatternMatch(_ % 2 == 1))  // false
  
  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    if(xs.length == 0) None
    else {
      //val m: Double = xs.foldRight(0.0)(_ + _) / xs.length
      //Some(xs.map(d => math.pow(d - m, 2)).foldRight(0.0)(_ + _) / xs.length)
      Some( xs.map(d => math.pow(d, 2)).foldRight(0.0)(_+_) / xs.length - math.pow(xs.foldRight(0.0)(_+_) / xs.length, 2) )
    }
  }
  def varianceUsingFlatMap(xs: Seq[Double]): Option[Double] = {
    val len: Option[Double] = xs.length match {
      case 0 => None
      case n => Some(n)
    }
    
    val e_x2 = xs.map(d => math.pow(d, 2)).foldRight(0.0)(_+_)
    val ex_2 = xs.foldRight(0.0)(_+_)
    
    len.flatMap(l => Some(e_x2 / l - math.pow(ex_2 / l, 2)) )
  }
  
  val s1 = Seq(1.0, 2.0, 5.0, 4.0, 8.0)
  val s2 = Seq()
  println("variance(Some): " + variance(s1))
  println("variance(None): " + variance(s2))
  println("variance_flatMap(Some): " + varianceUsingFlatMap(s1))
  println("variance_flatMap(None): " + varianceUsingFlatMap(s2))
  
  
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    0.0
  }
  
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Option.Try(age.toInt)
    val optTickets: Option[Int] = Option.Try(numberOfSpeedingTickets.toInt)
    //insuranceRateQuote(optAge, optTickets)
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }
  
  // 4.4
  val ols = List(o1s, o2s, o3s)        // no None
  val oln = List(o1s, o2n, o2s, o3s)   // including None
  
  val seqs = Option.sequence(ols)
  val seqn = Option.sequence(oln)
  
  println("sequence(Some): " + seqs)
  println("sequence(None): " + seqn)
  
  // 4.5
  val ll = List(2, 4, 6, 8)
  val ll2 = List(1, 2, 3, 4, 5)
  val olls = Option.traverse(ll)( n => n match {
    case even if even % 2 == 0 => Some(even)
    case _ => None
  })
  val olls2 = Option.traverse2(ll)( n => n match {
    case even if even % 2 == 0 => Some(even)
    case _ => None
  })
  val olln = Option.traverse(ll2)( n => n match {
    case even if even % 2 == 0 => Some(even)
    case _ => None
  })
  val olln2 = Option.traverse2(ll2)( n => n match {
    case even if even % 2 == 0 => Some(even)
    case _ => None
  })
  
  val seqs_traverse = Option.sequenceUsingTraverse(ols)
  val seqn_traverse = Option.sequenceUsingTraverse(oln)
  
  println("traverse(Some) : " + olls)
  println("traverse2(Some) : " + olls2)
  println("traverse(None) : " + olln)
  println("traverse(None) : " + olln2)
  
  println("sequence_traverse(Some) : " + seqs_traverse)
  println("sequence_traverse(None) : " + seqn_traverse)
  
  // 4.6
  val el: Either[String, Int] = Left("error")
  val er: Either[String, Int] = Right(12345)
  val el2: Either[String, Int] = Left("fault")
  val er2: Either[String, Int] = Right(34567)
  
  println("map(right): " + er.map(_ * 2))
  println("map(left): " + el.map(_ * 2))
  println("flatMap(right->right): " + er.flatMap(r => Right(r*2)) )
  println("flatMap(right->left): " + er.flatMap(r => Left("none")) )
  println("flatMap(left->right): " + el.flatMap(l => Right(0)) )
  println("flatMap(left->left): " + el.flatMap(l => Left("none")) )
  println("orElse(right): " + er.orElse(er2))
  println("orElse(left): " + el.orElse(er2))
  println("map2(right+right): " + er.map2(er2)( (a, b) => a + b))
  println("map2(right+left): " + er.map2(el2)( (a, b) => a + b))
  println("map2(left+left): " + el.map2(el2)( (a, b) => a + b))
  println("map2(left+right): " + el.map2(er2)( (a, b) => a + b))
}

