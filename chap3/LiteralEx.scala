object MyApp extends App {
	//	Integer Literal
	val l1 = 1234567890123L	///	Long type. 뒤에 L 또는 l 을 붙인다.
	val l2 = 1234567890123l
	val b1: Byte = -128

	//	Floating Point Literal
	val d1 = .14	///	부동소수점 타입은 . 앞에 0개 이상의 숫자가 올수가 있다
	val f1 = .14F	///	F or f 를 붙이면 float type. 없으면 doblue type

	//	Boolean Literal
	val B1 = true

	//	Character Literal
	val c1 = 'A'
	val c2 = '\u0041'	///	유니코드 'A'
	
	//	String Literal
	val s1 = "Programming\nScala"
	val s2 = """  "Programming Scala", "this is triple '"' " """

	//	Symbol Literal... Interned String 으로서, 같은 심벌은 실제로 같은 메모리를 참조한다.
	val sb1 = 'aSymbol	///	그런데, 용도를 모르겠음...

	//	Function Literal
	val fn1: Function1[Int, String] = i => i.toString() 

	val fn2: Function2[Int, String, String] = (i, s) => s + i
	//	val fn2: (Int, String) => String 과 동일한 표현

	//	Tuple Literal
	val t1: (Int, String) = (1, "two")
	val t2: Tuple2[Int, String] = (1, "two")
	println("print the Tuple's first item. first: " + t2._1 + ", second: " + t2._2)

	//	원소가 2개인 Tuple 을 Pair 라고 한다.
	val p1 = (1, "one")
	val p2 = 1 -> "one"
}