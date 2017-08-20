class Foo(var x: Int, var y: Int) {

}


object Foo {
	var updated = 1

	def apply(x: Int, y: Int) = new Foo(x, y)

	def unapply(extractor: Foo) = {
		if (extractor.x == 0 || extractor.y == 0) None
		else Some(extractor.x, extractor.y)
	}

	def update(up: Int, foo: Foo) = {
		foo.x *= up
		foo.y *= up
	}
}


object MyApp {
	def main(args: Array[String]) {
		//	연산자는 메소드 호출을 간단하게 하기 위한 편의일 뿐이다.
		//	중위표기
		val sum = 1 + 2		///	val sum = (1).+(2) 와 동일함
		println("1 + 2 => " + sum)

		val s = "Hello World!"
		val index = s indexOf 'o'
		println("'o' index of \"" + s + "\" => " + index)

		//	전위표기 +, -, !, ~ 만 가능함
		println("-2.0 == " + (2.0).unary_-)

		//	후위표기	인자를 취하지 않는 메소드를 '.' 이나 () 없이 호출
		//val lower = s toLowerCase		컴파일시 경고. postfix import 필요. 

		//	산술연산, 관계 및 논리연산, 비트 연산, 연산자 우선순위 및 결합법칙 생략

		//	f(arg...) 가 우측에 있을 경우 apply 함수 호출
		val bar = Foo(10, 20)	///	val bar = Foo.apply(1,2) 와 같음
		println("class Foo's Parameter x: " + bar.x + ", y: " + bar.y)

		bar match {
			///	Foo.unapply(bar) 를 호출한 것과 같다.
			case Foo(x, y) => println("init class Foo's x: " + x + ", y: " + y)
			case _ => println("match error!")
		}

		//	f(arg...) 가 좌측에 있을 경우 update 함수 호출. 이 때 우측값이 갱신값 인자로 넘어온다.
		Foo(2) = bar		///	Foo.update(bar) 와 같음. bar 의 필드를 *2 만큼 갱신한다.
		println("class Foo's member field 2X updated. x: " + bar.x + ", y: " + bar.y)
	}
}
