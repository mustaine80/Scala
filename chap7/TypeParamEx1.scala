//	functional Queue
//	기본적으로 generic type 은 무공변(nonvariant)이다.
class Queue[+T] private (	///	+T 는 공변성(covariant)을 요구한다.
	private[this] var leading: List[T],
	private[this] var trailing: List[T]
) {
	private def mirror() = 
		if (leading.isEmpty) {
			while (!trailing.isEmpty) {
				leading = trailing.head :: leading
				trailing = trailing.tail
			}
		}

	def head: T = {
		mirror()
		leading.head
	}

	def tail: Queue[T] = {
		mirror()
		new Queue(leading.tail, trailing)
	}

	//	제너릭 타입의 파라미터가 메소드 파라미터의 타입이라면 그 메소드를 포함하는 클래스나 트레이트는
	//	해당 타입 파라미터에 대해 공변적이지 않을 수 있다.

	//	이를 해결하기 위해 enqueue 를 다형성(enqueue 에 파라미터를 지정)으로 더 일반화하고
	//	타입 파라미터에 하위 바운드를 사용한다. 
	def enqueue[U >: T](x: U) = 	///	U 는 T 의 super type
		new Queue[U](leading, x :: trailing)

	override def toString =
		leading ::: trailing.reverse mkString ("Queue(", ", ", ")")
}

object Queue {
	def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}

object TypeParamEx1 extends App {
	//	타입 파라미터 변성 예
	val intQ = Queue(1,2,3)
	println(intQ.toString)

	val variantQ = intQ.enqueue("abc")
	println(variantQ.toString)
}