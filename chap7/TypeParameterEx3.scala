import scala.language.implicitConversions

//	view bound 예제. context bound 로 구현 가능하며, 사용 중단 논의가 진행 중이기 때문에
//	가급적 사용하지 않는 편이 좋다.
// object Serialization {
// 	case class Writable(value: Any) {
// 		def serialized: String = s"-- $value --"
// 	}

// 	implicit def fromInt(i: Int) = Writable(i)
// 	implicit def fromFloat(f: Float) = Writable(f)
// 	implicit def fromString(s: String) = Writable(s)
// }

// import Serialization._

// object RemoteConnection {
// 	def write[T <% Writable](t: T): Unit =	///	view bound
// 			println(t.serialized)
// }


//	context bound 예제. 위의 예제를 context bound 로 변경
object Serialization {
	case class Rem[A](value: A) {
		def serialized: String = s"-- $value --"
	}

	type Writable[A] = A => Rem[A]
	implicit def fromInt: Writable[Int] = (i: Int) => Rem(i)	///	Rem.apply(i)
	implicit def fromFloat: Writable[Float] = (f: Float) => Rem(f)
	implicit def fromString: Writable[String] = (s: String) => Rem(s)
}

import Serialization._

object RemoteConnection {
	def write[T: Writable](t: T): Unit = ///	context bound
			println(t.serialized)
}


object TypeParameterEx3 extends App {
	RemoteConnection.write(100)
	RemoteConnection.write(3.14f)
	RemoteConnection.write("hello!")
	//RemoteConnection.write((1, 2))	///	compile error. no implicit view
}