//	copied source from http://github.com/kamilduda/programming-in-scala-odersky

//	because of issue fixing, I copied source. "object creation impossible, since method apply in trait 
//	CanBuildFrom of type (from: RNA)scala.collection.mutable.Builder[Base,RNA] 
//	is not defined new CanBuildFrom[RNA, Base, RNA]"

abstract class RNABase

case object A extends RNABase

case object T extends RNABase

case object G extends RNABase

case object U extends RNABase

object RNABase {
  val fromInt: Int => RNABase = Array(A, T, G, U)
  // fromInt(1) returns T (elem at position 1) - Array's apply method usage

  val toInt: RNABase => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
  // toInt(G) returns 2 (map.get(key)) - Map's apply method usage
}

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.collection.mutable.ArrayBuffer


/**
  * RNA has 4 different values; therefore, it will be represented as 2 bytes.
  * Integer uses 32 bytes so it will fill 16 [[RNABase]]s objects.
  */

/**
  * RNA - fully functional (ready to use) collection
  *
  * @param groups RNA data with 16 [[RNABase]]s in each element
  * @param length number of [[RNABase]]s on the array (and in the RNA sequence)
  */
final class RNA private(val groups: Array[Int], val length: Int)
  extends IndexedSeq[RNABase] with IndexedSeqLike[RNABase, RNA] {
  // IndexedSeqLike[element, collection representation]
  // collection representation is the return type of methods such as take, drop filter etc.
  // IndexedSeqLike[Int, List[Int]] - example

  import RNA._

  /**
    * override from [[IndexedSeq]]
    */
  override def apply(idx: Int): RNABase = {
    if (idx < 0 || length < 0) throw new IndexOutOfBoundsException
    // val elem = group(index / 16) - get RNA from group array (each element holds 16 so divide by 16 to idx value)
    // shift elem right and extract it using mask
    RNABase.fromInt(groups(idx / Capacity) >> (idx % Capacity * Size) & Mask)
  }

  /**
    * override required by IndexedSeqLike[RNABase, RNA]
    * This method creates a builder of the right kind to be used by [[IndexedSeqLike]].
    * Class that extends XLike[+A, +Repr] must implement a builder with the SAME type!
    * mapResult creates new builder
    */
  override protected[this] def newBuilder: mutable.Builder[RNABase, RNA] = RNA.newBuilder

  /**
    * Override the default implementation with more officiant one.
    * Default implementation is ok but it can be better in this case.
    * It is worth to have foreach as good as we can because it is used in most methods.
    */
  override def foreach[U](f: RNABase => U): Unit = {
    var i = 0
    var b = 0
    while (i < length) {
      b = if (i % Capacity == 0) groups(i / Capacity)
      else b >>> Size
      f(RNABase.fromInt(b & Mask))
      i += 1
    }
  }
}

object RNA {


  /**
    * Number of bits necessary to represent a group
    */
  private val Size = 2

  /**
    * Number of groups tht fit in an Int
    */
  private val Capacity = 32 / Size

  /**
    * Mask to isolate a group
    * 0001
    * 0100
    * 0011
    */
  private val Mask = (1 << Size) - 1

  /**
    * So we can use RNA(T, G, A, A)
    */
  def apply(bases: RNABase*): RNA = fromSeq(bases)

  /**
    * Convert from the sequence of RNA element into our RNA collection
    */
  def fromSeq(xs: Seq[RNABase]): RNA = {
    // size not to big, enough fit all elements
    val groups = new Array[Int]((xs.length + Capacity - 1) / Capacity)
    for (i <- xs.indices) groups(i / Capacity) |= RNABase.toInt(xs(i)) << (i % Capacity * Size)
    new RNA(groups, xs.length)
  }

  /**
    * For use in concrete class
    */
  def newBuilder: mutable.Builder[RNABase, RNA] = new ArrayBuffer[RNABase] mapResult fromSeq

  /**
    * Implicit method needed for map or ++ methods and many other.
    * See map definition:
    * def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That
    */
  implicit def canBuildFrom: CanBuildFrom[RNA, RNABase, RNA] = {
    new CanBuildFrom[RNA, RNABase, RNA] {
      // create a new builder for RNA collection
      override def apply(): mutable.Builder[RNABase, RNA] = newBuilder

      // create a new builder for RNA collection
      override def apply(from: RNA): mutable.Builder[RNABase, RNA] = newBuilder
    }
  }

}


object ScalaArchitectureEx extends App {
	val moo = RNA(A,U,G,T,T)
	println(moo.length)
	println(moo.last)
	println(moo.take(3))	
	println(moo.filter(U != _))

	println(moo map RNABase.toInt)
	println(moo ++ List("missing", "data"))
	println(moo ++ moo)
}