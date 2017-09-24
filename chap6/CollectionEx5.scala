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
}