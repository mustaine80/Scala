object MapAndTupleEx {
	def main(args: Array[String]) 
	{
		//	기본 Map 사용
		val valMap = Map("hello" -> 1, "world" -> 2)
		valMap foreach println
		println("key: 'hello', value: " + valMap("hello"))

		//	mutable Map 사용
		import scala.collection.mutable
		val varMap = mutable.Map.empty[String, Int]	///	val 속성은 mutable Map 에 대해 적용된다.
		varMap("mutable hello") = 10
		varMap("mutable world") = 20
		varMap foreach println

		//	변경 불가능한 Map -> 변경 가능한 Map
		val mutableMap = mutable.Map.empty[String, Int] ++= valMap
		mutableMap("now mutable") = 3
		mutableMap foreach println

		//	변경 가능한 Map -> 변경 불가능한 Map
		val immutableMap = Map.empty ++ mutableMap
		// immutableMap("ever park") = 30

		//	map 에 없는 key 를 사용하면 None: Option 이 반환된다.
		val retOption = valMap.get("hi")
		retOption match {
			case None => println("key[hi]'s value is None: Option")
			case _ => retOption	/// Some
		}

		//	튜플 사용. 일반적인 사용 예는 여러 값을 반환하는 것이다.
		val words = "The quick red fox"
		val longest = longestWord(words.split(" "))
		println(words + "'s longest word & index [Tuple] : " + longest)

		//	튜플 사용법
		//	결합에 의미가 있거나 메소드를 추가해야 한다면 클래스를 생성하는 편이 좋다.
		println("longest word : " + longest._1)
		println("longest word's index : " + longest._2)
		val (word, idx) = longest
		println("longest word another : " + word)
		println("longest word's index another : " + idx)
	}	

	def longestWord(words: Array[String]) = {
		var word = words(0)
		var idx = 0
		for (i <- 1 until words.length)
			if (words(i).length > word.length) {
				word = words(i)
				idx = i
			}	
		(word, idx)	/// 최종 반환값을 튜플로 묶는다.
	}
}