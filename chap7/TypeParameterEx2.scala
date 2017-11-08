class CSuper {
	def msuper() = println("CSuper")
}

class C extends CSuper {
	def m() = println("C")
}

class CSub extends C {
	def msub() = println("CSub")
}

object TypeParameterEx2 extends App {
	//	trait Function1[-T1, +R] extends AnyRef
	
	//	var f: C => C = new Function1[C, C] {
	// 	def apply(c: C): C = new C
	// }

	//	C => C 가 의미하는 바는 올바른 C 값을 f 에 넘길 수 있고
	//	f 는 C 가 아닌 다른 것을 결코 반환하지 않는다는 계약을 정의함
	var f: C => C = (c: C) => new C
	
	f = (c: CSuper) => new CSub		/// 입력 반공변: CSuper 가 C 대체, 출력 공변: CSub 가 C 대체	
	f = (c: CSuper) => new C 		///	입력 반공변. ok
	f = (c: C) => new CSub			///	출력 공변. ok

	// f = (c: CSub) => new CSuper	///	컴파일 오류. CSub 가 C 대체 불가. CSuper 가 C 대체 불가
}