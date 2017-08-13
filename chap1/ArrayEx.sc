object ArrayEx {
	def main(args: Array[String]) 
	{
		//	기본 배열 생성, 초기화
		val arrs1 = new Array[Int](5)
		var count = 0
		while (count < 5) {
			arrs1(count) = count
			println("array index access arrs : " + arrs1(count))
			count += 1
		}

		val arrs2 = Array(0, 1,2,3,4)
		for (arr <- arrs2) println("array sequence arrs : " + arr)

		//	다차원 배열
		//	Array.ofDim 의 경우 5차원까지 지원한다. 그런데, 뭐 2차원 이상 쓸일이 있나?
		var arrs3 = Array.ofDim[String](2,3)
		arrs3(0)(0) = "a"
		arrs3(0)(1) = "b"
		arrs3(0)(2) = "c"
		arrs3(1)(0) = "d"
		arrs3(1)(1) = "e"
		arrs3(1)(2) = "f"

		for {
			i <- 0 until 2
			j <- 0 until 3
		} println(s"($i)($j) = Array.ofDim : ${arrs3(i)(j)}")

		//	Array of Array 도 가능하다.
		var arrs4 = Array(
				Array("g", "h", "i"),
				Array("j", "k", "l")
			)

		for {
			i <- 0 until 2
			j <- 0 until 3
		} println(s"($i)($j) = Array of Array : ${arrs4(i)(j)}")

		//	ArrayBuffer 는 모든 배열 메소드를 사용할 수 있으면서, 배열의 끝 또는 시작에 
		//	원소를 추가/삭제할 수 있으며, toArray 를 이용해 일반 배열로 변경할 수 있다.
		import scala.collection.mutable.ArrayBuffer
		val buf = new ArrayBuffer[Int]()
		buf += 1
		buf += 2
		buf += 3
		buf foreach println
	}
}