class Foo(x: Int, y: Int) {
	require(x != 0)		///	조건을 만족하지 않으면 인스턴트가 생성되지 않는다.
	private val bar = 0

	def apply = println("class's apply run.")

	def this(n: Int) = this(n, 100)	///	보조 생성자

	def printFoo = println("Foo x: " + x + " y: " + y)
}

object Foo {
	def apply(): Foo = {
		println("companion object's apply run.")
		val foo = new Foo(10)	///	보조 생성자를 이용한다.
		println("companion object can access private. value : " + foo.bar)
		return foo
	}
}

object singleton {
	private var sum = 0 

	def staticAdd(x: Int): Int = {
		//	x = x + 1	///	object parameter "val". reassignment fail
		sum += x
		sum
	}

	def apply(x: Int): Unit = {
		println("object singleton apply run. parameter : " + x)
	}
}

object MyApp extends App {	///	object 는 클래스/트레이트를 상속할 수 있다.
	println(singleton.staticAdd(1))	///	정적 메소드처럼 사용할 수 있다
	singleton(10)	///	object 의 apply 가 실행된다.

	//val bar = new Foo(0, 30)	///	require() 에 의해 인스턴스 생성이 실패한다.

	//	todo: apply 명시하지 않아도 되지 않나?
	val foo = Foo.apply	///	companion object 의 apply 가 실행된다.
	foo.apply	///	class instance 의 apply 가 실행된다.
	foo.printFoo
}

