//	Sequence Trait
object CollectionEx extends App {
	val xs = "hello world.  abcd"
	val ys = "lo"

	//	인덱스와 길이
	println(s"xs(2) : $xs(2)")
	println(s"xs isDefinedAt 5 : ${xs isDefinedAt 5}")
	println(s"xs.length : ${xs.length}")
	println(s"xs.lengthCompare(6) : ${xs.lengthCompare(6)}")

	//	인덱스 검색
	println(s"xs indexOf 'l' : ${xs indexOf 'l'}")
	println(s"xs lastIndexOf 'l' : ${xs lastIndexOf 'l'}")
	println(s"xs indexOfSlice ys : ${xs indexOfSlice ys}")
	println(s"xs lastIndexOfSlice ys : ${xs lastIndexOfSlice ys}")
	println(s"xs indexWhere (_ < 'e') : ${xs indexWhere (_ < 'e')}")

	///	ToDo: _ < 'e' 일 경우 왜 7 이 아니라 0 인건가? prefixLength 도 이해가 가지 않음
	println(s"xs segmentLength (_ > 'e', 6) : ${xs segmentLength (_ > 'e', 6)}")
	println(s"xs prefixLength (_ <= 'o') : ${xs prefixLength (_  <= 'o')}")

	//	 추가
	println(s"'n' +: xs : ${'n' +: xs}")
	println(s"xs :+ 'n' : ${xs :+ 'n'}")
	println(s"xs padTo (80, '-') : ${xs padTo (80, '-')}")

	//	변경
	println(s"xs patch (6, ys, 20) : ${xs patch (6, ys, 20)}")
	println(s"xs updated (4, ' ') : ${xs updated (4, ' ')}")

	///	seq(i) = x 는 update 와 동일하지만 mutable.Seq 만 사용 가능하다.
	import scala.collection.mutable.ListBuffer
	val ls = ListBuffer(1,2,3,4,5)
	println(s"ls(4) = 6 ${ls(4) = 6}")

	//	정렬
	println(s"xs sorted : ${xs sorted}")
	println(s"xs sortWith ((x,y) => x > y) : ${xs sortWith ((x,y) => x > y)}")
	println(s"ls sortBy (_ % 2) : ${ls sortBy (_ % 2)}")

	//	반전
	println(s"xs reverse : ${xs reverse}")
	println(s"xs reverseIterator next : ${xs.reverseIterator.next}")
	println(s"ls reverseMap (_ * -1) : ${ls reverseMap (_ * -1)}")

	//	비교
	println(s"xs startsWith hell : ${xs startsWith "hell"}")
	println(s"xs endsWith abcd : ${xs endsWith "abcd"}")
	println(s"xs contains ' ' : ${xs contains ' '}")
	println(s"xs containsSlice ys : ${xs containsSlice ys}")

	val a = List(2,4,6,8,10)
	val b = List(12,14,16,18,20)

	//	TODO: corresponds 이후부터 추후 작성
}