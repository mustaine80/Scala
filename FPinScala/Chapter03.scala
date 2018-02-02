sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
  
  // 3.1
  def num3_1() {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    
    println(x)
  }
  
  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, t) => t
      case Nil => Nil
    }
  }
  
  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Cons(head, tail) => Cons(h, tail)
      case Nil => Nil
    }
  }
  
  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(l: List[A], n: Int): List[A] = {
      if(n == 1) l match {
        case Nil => Nil
        case Cons(h, t) => t
      }
      else l match {
        case Nil => Nil
        case Cons(h, t) => go(t, n - 1)
      }
    }
    
    go(l, n)
  }
  
  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(ll: List[A]): List[A] = {
      ll match {
        case Cons(h, t) => if(f(h)) go(t) else ll
        case Nil => Nil
      }
    }
    
    go(l)
  }
  
  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(ll: List[A]): List[A] = {
      ll match {
        case Cons(h, t) => if(f(h)) go(t) else ll
        case Nil => Nil
      }
    }
    
    go(l)
  }
  
  // 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, Cons(t, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))      
      case Nil => Nil
    }
  }
  
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }    
  }
  
  
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f) )
    }
  }
  
  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  
  // 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }
  
  // 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }
  // 3.10_2
  def foldLeftTail[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => loop(t, f(z, h))(f)        
      }
    }
    
    loop(as, z)(f)
  }
  
  // 3.11
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  
  // 3.12
  def reverse[A](l: List[A]): List[A] = {    
    foldLeft(l, Nil:List[A])( (t: List[A], h: A) => Cons(h, t) )      
  }
  
  // 3.13
  /*
  def foldLeftUsingRight[A](l: List[A], z: B)(f: (B, A) => B): B = {
    
  }
  */
  /*
  def foldRightUsingLeftTail[A](l: List[A], z: B)(f: (A, B) => B): B = {
   
  }*/
  
  // 3.14
  def appendUsingFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)( Cons(_, _) )
  }
  
  // 3.15
  def appendLists[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil:List[A])( (l: List[A], z: List[A]) => List.append(l, z) )
  }
  
  // 3.16
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])( (h: Int, xs: List[Int]) => Cons(h + 1, xs) )
  }
  
  // 3.17
  def convertToString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])( (h: Double, xs: List[String]) => Cons(h.toString() + "s", xs) ) 
  }
  
  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])( (h: A, xs: List[B]) => Cons(f(h), xs) )
  }
  
  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])( (h: A, xs: List[A]) => {
      if( f(h) ) Cons(h, xs)
      else xs
    })
  }
  
  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])( (h: A, xs: List[B]) => append(f(h), xs) )
  }
  
  // 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if(f(x)) List(x) else Nil)
  }
  
  // 3.22
  def zipTwoList(a1: List[Int], a2: List[Int]): List[Int] = {
    //@annotation.tailrec
    def loop(l1: List[Int], l2: List[Int]): List[Int] = {
      l1 match {
        case Nil => Nil
        case Cons(h, t) => {
          l2 match {
            case Cons(h2, t2) => Cons(h + h2, loop(tail(l1), tail(l2)) )
            case _ => Nil
          }
        }
      }      
    }
    
    loop(a1, a2)
  }

  // 3.23
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = {
    def loop(l1: List[A], l2: List[A]): List[A] = {
        l1 match {
          case Nil => Nil
          case Cons(h, t) => {
            l2 match {
              case Cons(h2, t2) => Cons(f(h, h2), loop(tail(l1), tail(l2)) )
              case _ => Nil
            }
          }
        }      
      }
      
      loop(a1, a2)
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    false
  }
}


object Chapter03 extends App {
  // 3.1
  List.num3_1()
  
  val l = List(2, 3, 4, 5, 11, 2, 4, 8, 9, 0)
  val l2 = List(22, 33, 44)
  val dl = List(1.0, 1.2, 2.3)
  
  // 3.2
  println(List.tail(l))
  
  // 3.3
  println(List.setHead(l, 1))
  
  // 3.4, 3.5
  println(List.drop(l, 2))
  println(List.dropWhile(l, (i: Int) => i != 11))
  println(List.dropWhileCurried(l)(i => i != 11))
  println(List.dropWhileCurried(l)(_ != 11))
    
  // 3.6
  println(List.init(l))
  
  println(List.sum2(l))
  println(List.product2(dl))
  
  // 3.8
  println(List.foldRight(dl, Nil:List[Double])(Cons(_, _)))
  
  // 3.9
  println(List.length(l))
  
  // 3.10
  println(List.foldLeft(dl, 1.0)(_ * _))
  println(List.foldLeftTail(dl, 1.0)(_ * _))
  println(List.foldLeft(l, 0)(_ + _))
  println(List.foldLeftTail(l, 0)(_ + _))
  
  // 3.11
  println(List.sumLeft(l))
  println(List.productLeft(dl))
  
  // 3.12
  println(List.reverse(l))
  
  
  // 3.14
  println(List.appendUsingFold(l, l2))
  
  // 3.15
  println(List.appendLists( List(List(1,2,3), List(4,5,6), List(7, 8, 9) ) ) )
  
  // 3.16
  println(List.addOne(l))
  
  // 3.17
  println(List.convertToString(dl))
  
  // 3.18
  println(List.map(dl)(_.toString()))
  
  // 3.19
  println(List.filter(l)(_ % 2 == 0))
  
  // 3.20
  println(List.flatMap(l)(i=>List(i, i)) )
  
  // 3.21
  println(List.filterUsingFlatMap(l)(_ % 2 == 0))
  
  // 3.22
  println(List.zipTwoList(List(1,2,3), List(4,5,6)) )
  
  // 3.23
  println(List.zipWith( List(1,2,3), List(4,5,6) )( _ + _ ))
}