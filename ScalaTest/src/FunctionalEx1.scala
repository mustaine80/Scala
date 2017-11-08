object MultiTable {
	//	곱셈표 출력. 명령행 스타일
	//	곱셈표를 순차적으로 출력해 나감 (패딩 출력 -> 곱셈결과 출력 -> 단위행 출력 -> 전체행 출력)
	def printMultiTableImperative() {
		var i = 1
		while (i <= 10) {
			var j = 1
			while (j <= 10) {
				val prod = (i * j).toString
				var k = prod.length
				while (k < 4) {
					print(" ")
					k += 1
				}

				print(prod)
				j += 1
			}

			println()
			i += 1
		}
	}

	//	곱셈표 출력. 함수형 스타일
	//	부수효과 없이 곱셈표를 구성해 나감 (전체행 += 단위행 += 패딩 + 곱셈결과)
	def makeRowSeq(row: Int) =
		for (col <- 1 to 10) yield {
			val prod = (row * col).toString
			val padding = " " * (4 - prod.length)
			padding + prod
		}

	def makeRow(row: Int) = makeRowSeq(row).mkString

	def printMultiTableFunctional() = {
		val tableSeq =
			for (row <- 1 to 10)
				yield makeRow(row)

		tableSeq.mkString("\n")
	}
}

object ClousreEx {
	def makeAddMore(more: Int) = (x: Int) => x + more
}

object RecursionEx {
	def approximate(guess: Int): Int = {
		if (isGoodEnough(guess)) guess
		else approximate(improve(guess))
	}

	def isGoodEnough(guess: Int): Boolean = {
		if (guess > 1000000) true
		else false
	}

	def improve(guess: Int): Int = guess + 1

	import scala.annotation.tailrec
	@tailrec
	def boom(x: Int): Int = {
		if (x == 0) throw new Exception("Boom!!!")
		else boom(x - 1)
	}

	import scala.util.control.TailCalls._
	def isEven(x: Int): TailRec[Boolean] = {
		if (x == 0) done(true)
		else tailcall(isOdd(x - 1))
	}

	def isOdd(x: Int): TailRec[Boolean] = {
		if (x == 0) done(false) 
		else tailcall(isEven(x - 1))
	}
}

object MainFunctionalEx1 extends App {
	println("곱셈표 명령행 스타일 출력")
	MultiTable.printMultiTableImperative()

	println("\n곱셈표 함수형 스타일 출력")
	println(MultiTable.printMultiTableFunctional)

	//	함수는 1급 개체이다.
	//	함수를 값으로 사용할 수 있다.
	val increase = (x: Int) => x + 1
	printf("function is first class. function increase(1) 의 결과는 %d 이다.\n", increase(1))

	//	위치지정자(Place Holder)를 사용할 수 있다.
	println(List(1,2,3,4,5).filter(_ > 3))
	
	//	인자 추론을 실패할 경우에는 명시하면 된다.
	val add = (_: Int) + (_: Int)
	println(add(5, 10))

	//	부분적용함수
	val sum = (a: Int, b: Int, c: Int) => a + b + c
	val partiallySum = sum(1, _: Int, 3)
	println("부분적용 함수 결과 : " + partiallySum(2))

	//	클로저(closure). more 는 자유 변수이며 x 는 바운드 변수이다.
	var more = 1
	val addMore = (x: Int) => x + more
	println("more = 1 일때 addMore(1) 의 결과는 " + addMore(1))
	more = 10
	println("more = 10 일때 addMore(1) 의 결과는 " + addMore(1))

	//	함수를 호출할 때마다 새로운 클로저를 만들수도 있다.
	println("more = 100 일때 addMore(1) 의 결과는 " + ClousreEx.makeAddMore(100)(1))

	//	함수형 프로그래밍은 재귀를 많이 사용하게 된다. 이 때 발생할 수 있는 StackOverFlow를 피하기 위해
	//	스칼라는 꼬리재귀, 트램폴린 등의 방법을 제공한다.

	//	꼬리 재귀(Tail Recursion)
	//	재귀 호출마다 새로운 스택을 만들지 않고, 같은 스택 프레임을 재활용한다.
	//	단, 동일한 함수를 직접 재귀 호출하는 경우에만 해당한다.
	println("꼬리 재귀. 최적화된 숫자는? " + RecursionEx.approximate(1))

	//	트램펄린(Trampoline) stack overflow 를 막기 위해 루프 최적화를 한다.
	println("트램폴린. 10000000 은 짝수인가? " + RecursionEx.isEven(10000000).result)
}