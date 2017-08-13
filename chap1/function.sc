import scala.io.Source

object FunctionEx {
	def main(args: Array[String])
	{
		processFile("chap1_study.sc", 45)

		//	1급 함수
		List(0,1,2,3) foreach ((x: Int) => println(x + 1))	/// 함수리터럴

		var more = 1
		val addMore = (x: Int) => x + more	/// 클로저
		println("closure + 1: " + addMore(0))

		more = 10	///	자유변수값이 갱신되면 클로저도 같이 갱신된다.
		println("closure + 10: " + addMore(0)) 

		//	자유변수값은 클로저 생성시 결정된다.
		def makeIncreaser(i: Int) = (x: Int) => x + i
		println("+ 1 적용한 Increaser. i : " + makeIncreaser(1)(1))
		println("+ 9999 적용한 Increaser. i : " + makeIncreaser(9999)(1))
	
		//	부분 적용 함수
		val partialApply = sum(1, _: Int)
		println("sum(1, _: Int) 결과는 " + partialApply(2) + " 입니다.")

		//	가변 인자
		def echo(args: String*) =	///	String 반복 가능 (*). Array[String] 타입
			for (arg <- args) println(arg)
		echo("seok ki!", "nice to meet you.", "???")

		//	이름 붙인 인자
		def speed(distance: Float, time: Float): Float = distance / time
		println("named arguments. speed : " + speed(time=10.0f, distance=25.0f))

		//	디폴트 인자
		def printTime(out: java.io.PrintStream = Console.out) =
			out.println("default argument Console out. time = " + System.currentTimeMillis())
		printTime()
	}

	//	객체의 멤버 함수. "method"
	def processFile(fileName: String, width: Int) {
		//	함수 내에서 다시 함수를 정의할 수 있다.
		def processLine(line: String)
		{
			if (line.length > width)
				println(fileName + ": " + line.trim)
		}

		val source = Source.fromFile(fileName)
		for (line <- source.getLines())
			processLine(line)
	}

	//	부분 적용 함수
	def sum(x: Int, y: Int) = x + y
}