object CollectionEx extends App {
	val xs = List(1,2,3,4,5)

	//	Iterable Trait
	//	이터레이터
	xs.iterator foreach println
	xs grouped 3 foreach println
	xs sliding 3 foreach println

	//	하위 컬렉션
	xs takeRight 3 foreach println	///	xs 의 마지막 n개 원소로 구성된 컬렉션
	xs dropRight 3 foreach println	///	takeRight n 에 들어간 원소들을 제외한 나머지로 구성된 컬렉션

	//	묶기
	val ys = List("one", "two", "three", "four")
	xs zip ys foreach println	///	같은 위치에 있는 원소를 묶어서 쌍을 반환
	xs zipAll (ys, 0, "five") foreach println	///	zip 과 동일한데, xs와 ys의 길이가 다를 경우 길이를 맞춤
	ys.zipWithIndex foreach println

	//	비교
	val zs = List(1,2,3,4,5)
	if (xs sameElements zs) println("xs, zs 는 같은 원소를 같은 순서로 가지고 있습니다.")	
}