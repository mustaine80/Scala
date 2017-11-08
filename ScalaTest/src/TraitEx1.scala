
//	트레이트의 대표적인 활용 2가지
//	thin interface 를 rich interface 로 혹장
//	stackable(쌓을 수 있는) 변경

case class Point(x: Int, y: Int)

//	rich interface 예. 다른 클래스들이 트레이트를 mix-in 하여 구현된 인터페이스를 사용할 수 있다.
trait Rectangular {
  def topLeft: Point
  def bottomRight: Point

  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}

//  Rectangular Trait에서 이미 기능을 구현했기 때문에 별도로 구현할 필요가 없다
class Rectangle(val topLeft: Point, val bottomRight: Point) extends Rectangular

//  Stackable Interface 예
//  기존 Queue 클래스의 동작을 정의하는 변경을 Trait로 구현한 후 mix-in 하여 변경을 쌓아올린다.
abstract class IntQueue {
  def get(): Int
  def put(x: Int)
}

import scala.collection.mutable.ArrayBuffer

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int) {
    buf += x
  }
}

trait Doubling extends IntQueue {	//	이 트레이트는 IntQueue 를 상속한 클래스에서만 사용 가능하다.
  //	abstract override 수식자는 클래스 멤버에는 사용할 수 없고, 트레이트 멤버에만 사용할 수 있다.
  //	이 트레이트는 반드시 abstract method 에 대한 구체적 구현을 제공하는 클래스에 mix-in 해야 한다.
  abstract override def put(x: Int) {
    super.put(2 * x)	//	super 는 IntQueue 가 아닌 mix-in 된 클래스를 의미한다.
  }
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) {
    super.put(x + 1)
  }
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) {
    if (x >= 0) super.put(x)
  }
}

//	모든 수를 2배로
class DoublingQueue extends BasicIntQueue 
with Doubling

//	음수를 걸러낸 후 모든 수에 1을 더함
class FilteringIncrementingQueue extends BasicIntQueue 
with Filtering 
with Incrementing

object TraitEx extends App {
  val foo = new Rectangle(new Point(3,4), new Point(4,5))
  println("--------------------------------")
  println(foo.left)
  println(foo.right)
  println(foo.width)

  println("--------------------------------")
  val bar = new BasicIntQueue
  bar.put(10)
  bar.put(20)
  println(bar.get())	//	print 10
  println(bar.get())	//	print 20

  println("--------------------------------")
  val car = new DoublingQueue
  car.put(10)
  println(car.get())	//	print 20

  println("--------------------------------")
  val dar = new FilteringIncrementingQueue
  dar.put(10)
  println(dar.get())	//	print 11
  dar.put(-1)
  println(dar.get())	//	print 0, 중첩 트레이트는 맨 오른쪽부터 적용된다. (Inc -> Filter)
}