//	추상 클래스
abstract class Element {
	def contents: Array[String]
	
	//	파라미터 없는 메소드
	//	어떤 메소드가 인자도 받지 않고 메소드가 속한 객체의 변경 가능한 필드를 읽기만 하는 경우 주로 사용
	def height: Int = contents.length	
	def width: Int = if (height == 0) 0 else contents(0).length

	def demo() = println("Element's implementation invoked")
}

class ArrayElement(val contents: Array[String]) extends Element {
	//	파라미터 필드 정의를 통해 불필요한 중복 회피
	//	def contents: Array[String] = conts	
	
	//	height, width 상속

	override def demo() = println("ArrayElement's implementation invoked")

	//	method finalize
	def finalDemo() = println("this is ArrayElement's final method")
}

class LineElement(s: String) extends ArrayElement(Array(s)) {
	//	concrete member 를 구현할 경우에는 override 수식자 필요
	//	단, abstract member 는 생략 가능
	override def width = s.length
	override def height = 1

	override def demo() = println("LineElement's implementation invoked")

	// override def finalDemo() = "this is LineElement's final method override try"
}

//	class finalize
final class finalElement(s: String) extends LineElement(s)
// class tryInheritanceFromFinalElement extends finalElement

class UniformElement(
	ch: Char,
	override val width: Int,
	override val height: Int
) extends Element {
	private val line = ch.toString * width
	def contents = Array.fill(height)(line)	//	height 만큼 line 값으로 채운다.

	//	def demo() 상속
}

object MyApp {
	def invokeDemo(e: Element) {
		e.demo()
	}

	def main(args: Array[String]) {
		//	polymorphism example
		invokeDemo(new ArrayElement(Array("array")))
		invokeDemo(new LineElement("line"))
		invokeDemo(new UniformElement('!', 0, 0))
	}
}
