//	control structure

import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

object ControlEx {
	def main(args: Array[String]) {

//	스칼라 if 는 값을 가진다. c++ 도 삼항연산자로 지원은 가능하지만, 스칼라는 if 결과로 어떤 것이든 반환이 가능함
//	c++ :: string filename = !(args.isEmpry()) ? args(0) : "default.txt"; 
		val filename =
			if (!args.isEmpty) args(0)	/// String
			else 0.0	///	Double

		println("String or None-String? : " + filename)

//	또한, 부수효과가 없기 때문에 변수명을 표현식으로 대체할 수 있다.
		println(if (!args.isEmpty) args(0) else "default.txt")

//	스칼라 while, do-while loop 구조는 수행 결과가 특정 값이 아닌 Unit
//	아래의 loop 는 반환 값이 없다. C++ loop 와 동일
//	순수한 함수형 언어에는 표현식은 있지만 loop 는 없다.
//	스칼라에는 loop 가 존재하는데, 때로는 명령형의 해법이 가독성이 뛰어나기 때문이다.
//	하지만 loop 는 var 와 유사하기 때문에 이를 권장하지 않음
		println("gcd loop style : " + gcdLoop(100,25))	
		println("gcd recursive style : " + gcdRecursive(100,25))

//	스칼라 for 는 표현식으로 사용할 수 있다.
//	1. collection 순회. modern c++ 도 동일하게 지원한다.
		for (file <- filesHere) println("for Iteration. file list : " + file)

		for (i <- 1 to 5) println("for Iteration. range-base for : " + i)

//	2. for 표현식에 필터를 추가할 수 있다. 
//	for 문이 표현식이라고 불리는 이유는 사용하기 위한 값을 결과로 내놓기 때문이다.
		for (file <- filesHere if file.getName.endsWith(".sc"))
			println("for with Filter. '*.sc' file list : " + file)

//	3. generator 를 여러개(overlap) 사용할 수도 있고, 표현식 수행 중 변수 바인딩도 가능하다.
		grep(".*gcd.*")

//	4. 새로운 collection 을 만들어 낼 수 있다.
		val forLineLengths = 
			for {
				file <- filesHere
				if file.getName.endsWith(".sc")
				line <- fileLines(file)
				trimmed = line.trim
				if trimmed.matches(".*for.*")
			} yield trimmed.length

		for (length <- forLineLengths)
			println("for yield. line length : " + length)

//	try-cath-finally
		try {
			val f = new FileReader("input.txt") 
		} catch {
			case ex: FileNotFoundException => 
			case ex: IOException => 
		} finally {
			println("FileReader try error. finally 'input.txt' file must be close")
		}

//	match 표현식은 C++ 이 정수형 상수 타입만 사용할 수 있는 것과 달리 case 내 어떤 종류의 상수도 사용할 수 있으며, 
//	표현식의 결과가 값이다.
		val foo = "Lee"

		val matchRet = 
			foo match {
				case "Kim" => "Doo Hwan"
				case "lee" => "sin"
				case "Lee" => "Seok Ki"
				case _ => "hul?"
			}
		println(foo + " " + matchRet)

//	스칼라는 break, continue 를 지원하지 않는다. 함수형 스타일에서는 값이 생성되지 않으면 의미가 없기 때문이다
		var i = 0
		var flag = false

		while (i < 5 && !flag) {
			if (i > 3) {
				if (i == 4) {	/// flag 를 조작하면 break 대용이 된다. i 가 4일때 break
					flag = true
					println("i == 4 ? i : " + i + ", no break. but, flag replace break.")
				}
			}	///	카운터를 제외한 모든 구문을 if 로 감싸면, continue 대용이 된다. i 가 3 이하 일때 continue
			println("i: " + i + ", no continue. but, if replace continue.")
			i += 1	///	하지만, 뭔가 어색하군...
		}

		println("원하는 것은 i 값 입니다. loop-style : " + i)

//	loop 보다 재귀 사용을 권장한다.
		println("원하는 것은 i 값입니다. recursive-style : " + recursiveForLoop(0))

//	scope 개념은 다른 언어와 동일하다.

//	마지막으로 함수형 제어구문 종합 sample
		println("이제 제어 구문 샘플 코드의 마지막 출력입니다.\n" + multiTable)
	}

	//	while loop
	def gcdLoop(x: Long, y: Long): Long = {
		var a = x
		var b = y

		while (a != 0) {
			val temp = a
			a = b % a
			b = temp
		}
		b
	}

	//	recursive
	def gcdRecursive(x: Long, y: Long): Long =
		if (y == 0) x else gcdRecursive(y, x % y)

	def filesHere = new java.io.File(".").listFiles

	def fileLines(file: java.io.File) =
		scala.io.Source.fromFile(file).getLines().toList

	//	for overlapping
	def grep(pattern: String) =
		for {
			file <- filesHere
			if file.getName.endsWith(".sc");
			line <- fileLines(file)
			trimmed = line.trim /// val binding
			if trimmed.matches(pattern)
		} println("for overlapping. " + file + ": " + trimmed)

	//	recursive replace loop
	def recursiveForLoop(i: Int): Int =
		if (i > 5) -1
		else if (i == 4) i + 1
		else recursiveForLoop(i + 1)

	//	functional style control structure
	def makeRowSeq(row: Int) = 
		for (col <- 1 to 10) yield {
			val prod = (row * col).toString
			val padding = " " * (4 - prod.length)
			padding + prod
		}

	def makeRow(row: Int) = makeRowSeq(row).mkString

	def multiTable() = {
		val tableSeq = for (row <- 1 to 10) yield makeRow(row)
		tableSeq.mkString("\n")
	}
}

