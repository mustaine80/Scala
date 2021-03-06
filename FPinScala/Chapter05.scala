package chapter05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
  }
  
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f) )
      case _ => z
    }    
  }
  
  def existsUsingFoldRight(p: A => Boolean): Boolean = {
    this.foldRight(false)((a, b) => p(a) || b)
  }
  
  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }
    
  // 5.1
  def toList: List[A] = {
    this match {
      case Cons(h, t) => List(h()) ++ t().toList
      case Empty => Nil
    }
  }
  
  // 5.2
  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] = {
      if(n == 1) s match {
        case Empty => Empty
        case Cons(h, t) => Stream.cons(h(), Empty)
      }
      else s match {
        case Empty => Empty
        case Cons(h, t) => Stream.cons(h(), go(t(), n - 1))
      }
    }
    
    go(this, n)
  }
  
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: => Stream[A], n: Int): Stream[A] = {
      if(n == 1) s match {
        case Empty => Empty
        case Cons(h, t) => t()
      }
      else s match {
        case Empty => Empty
        case Cons(h, t) => go(t(), n - 1)
      }      
    }
    
    go(this, n)
  }
  
  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A], p: A => Boolean): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => if( p(h()) ) Stream.cons(h(), go(t(), p)) else Empty
      }
    }
    
    go(this, p)
  }
  
  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)( (a, b) => p(a) && b )
  }
  
  // 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A])( (a, b) => if(p(a)) Stream.cons(a, b) else Empty )
  }
  
  // 5.6 (difficult)
  def headOptionUsingFoldRight : Option[A] = {
    //this.foldRight(None:Option[A])( (a, b) => { println("fold"); Some(a) } )
    this.foldRight(None:Option[A])( (a, b) => Some(a) )
  }
  
  // 5.7
  def map[B](f: A => B): Stream[B] = {
    //this.foldRight(Empty:Stream[B])( (a, b) => {println("map"); Stream.cons( f(a), b)} )
    this.foldRight(Empty:Stream[B])( (a, b) => Stream.cons( f(a), b) )
  }
  
  def filter(f: A => Boolean): Stream[A] = {
    //this.foldRight(Empty:Stream[A])( (a, b) => {println("filter"); if(f(a)) Stream.cons(a, b) else b} )
    this.foldRight(Empty:Stream[A])( (a, b) => if(f(a)) Stream.cons(a, b) else b )
  }
  
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    this.foldRight(s)( (a, b) => Stream.cons(a, b) )
  }
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Empty:Stream[B])( (a, b) => f(a).append(b) )
  } 
  
  
  // 5.13
  def mapUsingUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this)(s => s match {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    })
  }
  
  def takeUsingUnfold(n: Int): Stream[A] = {
    Stream.unfold( (this, n) )(tt => if(tt._2 > 0) {
      tt._1 match {
        case Cons(h, t) => Some(h(), (t(), tt._2 - 1))
        case _ => None
      }
      } else {
        None
    })
  }
  
  def takeWhileUsingUnfold(f: A => Boolean): Stream[A] = {
    Stream.unfold(this)(s => s match {
      case Cons(h, t) => if(f(h())) Some(h(), t()) else None
      case _ => None
      }
    )
  }
  
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold( (this, s2) )( tt => tt match {
      case ( Cons(h, t), Cons(h2, t2) ) => Some( f(h(), h2()), (t(), t2()) )
      case _ => None
    })
  }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold( (this, s2) )( tt => tt match {
      case ( Cons(h, t), Cons(h2, t2) ) => Some( (Some(h()), Some(h2())), (t(), t2()) )
      case ( Cons(h, t), Empty ) => Some( (Some(h()), None), (t(), Empty) )
      case ( Empty, Cons(h2, t2) ) => Some( (None, Some(h2())), (Empty, t2()) )
      case ( Empty, Empty ) => None
    })
  }
  
  // 5.14 (difficult)
  def startsWith[A](s: Stream[A]): Boolean = {
    val ss : Stream[Boolean] = Stream.unfold( (this, s) )( tt => tt match {
      case ( Cons(h, t), Cons(h2, t2) ) => if(h() == h2()) Some( true, (t(), t2()) ) else Some( false, (t(), t2()) ) 
      case ( Cons(h, t), Empty ) => None
      case ( Empty, Cons(h2, t2) ) => Some( false, (Empty, t2()) )
      case ( Empty, Empty ) => None
    })
    
    !ss.exists(_ == false)
  }
  
  def startsWith2[A](s: Stream[A]): Boolean = {
    val ss = zipWith(s)( (a, b) => a == b )
    
    !ss.exists(_ == false)
    // s가 this보다 길이가 길 경우를 만족하지 못할 수도 있음
  }
  
  // 5.15
  def tails : Stream[Stream[A]] = {
    Stream.unfold( (this, false) )( tt => tt match {
      case (Cons(h, t), false) => Some(tt._1, (t(), false)) 
      case (Empty, false) => Some(Empty, (Empty, true))
      case (Empty, true) => None
      case _ => None
    })
  }
    
  def hasSubsequence[A](s: Stream[A]): Boolean = { 
    tails exists (_ startsWith s)
  }
  
  // 5.16 (difficult)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    Stream.unfold( tails )( ss => ss match { 
      case Cons(h, t) => Some( (h().foldRight(z)(f), t()) )
      case _ => None
    })
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  
   // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = Stream.cons(a, c)
    
    c
    //Stream.cons(a, constant(a))
  }
  
  // 5.9
  def from(n: Int): Stream[Int] = {
    lazy val c: Stream[Int] = Stream.cons(n, c.map(_ + 1))
    
    c
  }
  
  // 5.10
  def fibs(): Stream[Int] = {
    lazy val c: Stream[(Int, Int)] = Stream.cons((0, 1), c.map( t => (t._2, t._1 + t._2) ) )
        
    c.map(_._1)
  }
  
  // 5.11
  def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    lazy val c: Stream[Option[(A, S)]] = Stream.cons(f(z), c.map( o => f(o.get._2) ) )
    
    c.takeWhile(o => o match {
      case Some(t) => true
      case None => false
    }).map(_.get._1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = { 
    f(z) match {
      case Some(t) => Stream.cons(t._1, unfold(t._2)(f))
      case None => Empty
    }
  }
  
  // 5.12
  def fibsUsingUnfold(): Stream[Int] = {
    unfold( (0, 1) )( t => Some(t._1 + t._2, (t._2, t._1 + t._2)) )
  }
    
  def fromUsingUnfold(n: Int): Stream[Int] = {
    unfold( n )(a => Some(a + 1, a + 1) )  
  }
  
  def fromTo(n: Int, m: Int): Stream[Int] = {
    unfold( n )(a => if(a <= m) Some(a, a+1) else None)
  }
  
  def constantUsingUnfold(n: Int): Stream[Int] = {
    unfold( n )(a => Some(a, a)) 
  }  
  
  def onesUsingUnfold(): Stream[Int] = {
    constantUsingUnfold(1)
  }
}

object Chapter05 extends App {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = 
    if(cond) onTrue() else onFalse()
    
  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if(cond) onTrue else onFalse
    
  val a = 30
  if2(a < 22, () => println("a"), () => println("b") )
  if3(a > 22, () => println("aa"), () => println("bb") )()
  
  
  def maybeTwice(b: Boolean, i: => Int) = if(b) i+i else 0
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if(b) j + j else 0
  }
  
  val x = maybeTwice(true, {println("hi"); 1+41})
  val x2 = maybeTwice2(true, {println("hi2"); 1+41})
  
  val o = Stream(1, 2, 3, 4).find(_ % 2 == 0)
  println(o)
 
  // 5.1
  val s1 = Stream(1, 2, 3, 4, 5)
  val s2 = Empty
  
  val slist1 = s1.toList

  println("5.1) Stream: " + s1)
  println("5.1) List : " + slist1)
  
  // 5.2
  val take2 = s1.take(2)
  println("5.2) take(2) : " + take2.toList)
  
  val drop3 = s1.drop(3)
  println("5.2) drop(3) : " + drop3.toList)
  
  // 5.3
  val takew = s1.takeWhile(_ != 3)
  println("5.3) takeWhile : " + takew.toList)
  
  val b1 = s1.exists(_ == 4)
  val b2 = s1.existsUsingFoldRight(_ == 6)
  
  println("exist : " + b1)
  println("exist using foldRight : " + b2)
  
  // 5.4
  val b3 = s1.forAll(_ < 6)  // true
  val b4 = s1.forAll(_ % 2 == 0)  // false
  println("5.4) forAll(true) : " + b3)
  println("5.4) forAll(false) : " + b4)
  
  // 5.5
  val takew2 = s1.takeWhileUsingFoldRight(_ != 3)
  println("5.5) takeWhile(foldRight) : " + takew2.toList)
  
  // 5.6
  val ho1 = s1.headOption
  val ho2 = s1.headOptionUsingFoldRight
  println("5.6) headOption : " + ho1)
  println("5.6) headOption(foldRight) : " + ho2)
  
  // 5.7
  val m1 = s1.map(_ * 3).toList
  println("5.7) map: " + m1)
  val f1 = s1.filter(_ % 2 == 0).toList
  println("5.7) filter : " + f1)
  //println(Stream(1,2,3,4).map(_+10).filter(_%2==0).toList)
  val a1 = s1.append(Stream(10, 20, 30))
  println("5.7) append : " + a1.toList)
  val fl1 = s1.flatMap(e => Stream(e, e))
  println("5.7) flatMap : " + fl1.toList)
  
  
  // infinite stream
  val ones: Stream[Int] = Stream.cons(1, ones)
  val ll = ones.take(5).toList
  println(ll)
  println(ones.exists(_ % 2 != 0))
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ != 1).toList)
  println(ones.forAll(_ != 1))
  
  // 5.8
  val tens = Stream.constant(10)
  println("5.8) constant: " + tens.take(3).toList)
  
  // 5.9
  val from = Stream.from(11)
  println("5.8) from: " + from.take(4).toList)
  
  // 5.10
  val fibs = Stream.fibs()
  println("5.9) fibs: " + fibs.take(10).toList)
  
  // 5.12
  val fibs2 = Stream.fibsUsingUnfold()
  println("5.12) fibs(unfold): " + fibs2.take(10).toList)
  val from2 = Stream.fromUsingUnfold(5)
  println("5.12) from(unfold): " + from2.take(6).toList)
  val const2 = Stream.constantUsingUnfold(2)
  println("5.12) constant(unfold): " + const2.take(4).toList)
  val ones2 = Stream.onesUsingUnfold()
  println("5.12) ones(unfold) : " + ones2.take(5).toList)
  
  // 5.13
  val m2 = s1.mapUsingUnfold(_ * 3).toList
  println("5.13) map: " + m2)
  val take3 = s1.takeUsingUnfold(3)
  println("5.13) take(unfold) : " + take3.toList)
  val takew4 = s1.takeWhileUsingUnfold(_ != 4)
  println("5.13) takeWhile(unfold) : " + takew4.toList)
  val zip = s1.zipWith(Stream(5, 4, 3, 2, 1, 0))(_ * _)
  println("5.13) zipWith : " + zip.toList)
  val zipAll = s1.zipAll(Stream(5, 4, 3, 2, 1, 0))
  println("5.13) zipAll : " + zipAll.toList)
  
  // 5.14
  val s123 = Stream(1, 2, 3)
  val s456 = Stream(4, 5, 6)
  println("5.14) " + s1.toList + " startsWith " + s123.toList + "? => " + s1.startsWith(s123))
  println("5.14) " + s1.toList + " startsWith " + s456.toList + "? => " + s1.startsWith(s456))
  
  // 5.15
  val tails = s1.tails
  println("5.15) " + tails.map(_.toList).toList)
  
  println("hasSubsequence : " + s1.hasSubsequence(Stream(3, 4)))  
  
  // 5.16
  println("5.16) scanRight : " + s1.scanRight(1)(_ + _).toList)
  
  println(Stream.fromTo(10, 15).toList)
}