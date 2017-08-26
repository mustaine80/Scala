object FileMatcher {
	private def filesHere = (new java.io.File(".")).listFiles
	
	//	파일 확장자가 쿼리와 일치할 경우 추출
	def filesEnding(query: String) =
		for (file <- filesHere; if file.getName.endsWith(query))
			yield file

	//	파일 이름이 쿼리와 일치할 경우 추출
	def filesContaining(query: String) = 
		for (file <- filesHere; if file.getName.contains(query))
			yield file

	//	파일 이름이 정규표현식 쿼리와 일치할 경우 추출
	def filesRegex(query: String) = 
		for (file <- filesHere; if file.getName.matches(query))
			yield file
}


object HigherOrderedFileMatcher {
	private def filesHere = (new java.io.File(".")).listFiles

	//	원하는 메소드를 호출하는 함수값을 인자로 전달
	// def filesMatching(query: String, matcher: (String, String) => Boolean) = {
	// 	for (file <- filesHere; if matcher(file.getName, query))
	// 		yield file
	// }

	///	_.endsWith(_) 는 자유 변수가 없는 함수값
	// def filesEnding(query: String) 		= filesMatching(query, _.endsWith(_))
	// def filesContaining(query: String)	= filesMatching(query, _.contains(_))
	// def filesRegex(query: String)		= filesMatching(query, _.matches(_))

	//	좀 더 리팩토링할 수 있다.
	private def filesMatching(matcher: String => Boolean) =
		for (file <- filesHere; if matcher(file.getName))
			yield file

	///	_.endsWith(query) 는 자유변수(query)를 가지는 클로저
	///	filesMatching 의 matcher 인자였던 query 를 생략할 수 있다.
	///	또한, 고차함수(filesMatching)를 이용해 아래의 API 코드 구현이 단순화되었다.
	def filesEnding(query: String)		= filesMatching(_.endsWith(query))
	def filesContaining(query: String)	= filesMatching(_.contains(query))
	def filesRegex(query: String)		= filesMatching(_.matches(query))
}

object Curring {
	def sum(x: Int)(y: Int) = x + y
}


import java.io._

object HigherOrderAndCurring {
	def withPrintWriter(file: File)(op: PrintWriter => Unit) {
		val writer = new PrintWriter(file)
		try {
			op(writer)
		} finally {
			writer.close()
		}
	}
}

object Main extends App {
	// import FileMatcher._
	import HigherOrderedFileMatcher._
	println("현재 디렉토리에서 확장자가 scala 인 파일 목록입니다. " + filesEnding(".scala").toList)
	println("현재 디렉토리에서 파일명에 'Basic'이 포함된 파일 목록입니다. " + filesContaining("Basic").toList)
	println("현재 디렉토리에서 파일명에 숫자가 포함된 파일 목록입니다. " + filesRegex(".*0-9.*").toList)

	///	고차함수의 또 다른 중요한 용도는 API 에 고차함수를 포함시켜 클라이언트 코드를 더 간결하게 만드는 것이다.
	def containsNeg(nums: List[Int]): Boolean = {
		var exists = false
		for (num <- nums) 
			if (num < 0) 
				exists = true
		exists
	}

	///	판단 로직을 제외하고 나머지는 containsNeg 와 동일하다.
	def containsOdd(nums: List[Int]): Boolean = {
		var exists = false
		for (num <- nums) 
			if (num % 2 == 1) 
				exists = true
		exists
	}

	///	고차함수인 exists 를 이용하여 클라이언트 코드가 간결해졌다.
	def HigherOrderedContainsNeg(nums: List[Int]): Boolean = nums.exists(_ < 0)
	println("List(-10,0,1,2,3) 에 음수가 있는가? " + HigherOrderedContainsNeg(List(-10,0,1,2,3)))

	def HigherOrderedContainsOdd(nums: List[Int]): Boolean = nums.exists(_ % 2 == 1)
	println("List(-10,0,1,2,3) 에 홀수가 있는가? " + HigherOrderedContainsOdd(List(-10,0,1,2,3)))

	///	Curring
	println("1 + 2 는 " + Curring.sum(1)(2))

	val onePlus = Curring.sum(1) _
	println("1 + 2 는 " + onePlus(2))

	val twoPlus = Curring.sum(2) _ 
	println("2 + 2 는 " + twoPlus(2))

	///	고차함수와 커링을 연계하여 제어 구조를 추상화할 수 있다.
	///	사용자 코드가 아닌 withPrintWriter 에서 파일 닫기를 보장한다. (loan pattern)
	import HigherOrderAndCurring._
	val file = new File("date.txt")
	withPrintWriter(file) {
		///	ToDo: 아래의 함수값을 by-name parameter 로 만들 수 있나?
		///	println 부수효과 없이 withPrintWriter 를 수행하고 싶은데
		writer => writer.println(new java.util.Date)	///	2nd argument
	}
}