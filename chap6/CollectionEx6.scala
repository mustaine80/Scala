object CollectionEx extends App {
	//	Buffer
	import scala.collection.mutable.ListBuffer
	val xs = ListBuffer(4,5)
	val emptyBuf = ListBuffer[Int]()

	//	추가
	emptyBuf += 1
	println(s"emptyBuf += 1 : ${emptyBuf}")

	emptyBuf += (2,3)
	println(s"emptyBuf += (2,3) : ${emptyBuf}")

	emptyBuf ++ xs
	println(s"emptyBuf ++ xs : ${emptyBuf}")

	0 +=: emptyBuf
	println(s"0 +=: emptyBuf : ${emptyBuf}")

	xs ++=: emptyBuf
	println(s"xs ++=: emptyBuf : ${emptyBuf}")

	emptyBuf insert (0,-2)
	println(s"emptyBuf insert (0, -2) : ${emptyBuf}")

	emptyBuf insertAll (0, xs)
	println(s"emptyBuf insertAll (0, xs) : ${emptyBuf}")

	//  제거
	emptyBuf -= -2
	println(s"buf -= -2 : ${emptyBuf}")

	emptyBuf remove 0
	println(s"buf remove 0 : ${emptyBuf}")

	emptyBuf remove (1,2)
	println(s"buf remove (1, 2) : ${emptyBuf}")

	emptyBuf trimStart 2
	println(s"buf trimStart 2 : ${emptyBuf}")

	emptyBuf trimEnd 2
	println(s"buf trimeEnd 2 : ${emptyBuf}")

	//	복사
	val bufClone = emptyBuf.clone
	println(s"buf clone : ${bufClone}")

	//	제거2
	emptyBuf.clear()
	println(s"buf.clear() : ${emptyBuf}")

	//	맵을 캐시로 사용하는 예
	def f(x: String) = {
		println("taking my time")
		Thread.sleep(100)	//	비용이 많이 드는 계산
		x.reverse
	}	

	val cache = collection.mutable.Map[String, String]()

	//	효율적인 캐시 함수 구현
	def cachedF(s: String) = cache.getOrElseUpdate(s, f(s))

	println(cachedF("abc"))	///	캐시 저장
	println(cachedF("abc"))	///	캐시 사용

	//	lazy evaluation 변환은 view 메소드에서만 가능
	//	이를 다시 eager evaluation 으로 변환하려면 force 메소드 사용
	val v = Vector(1,2,3,4,5)
	println(v.view map (_ + 1) map (_ * 2))
	println((v.view map (_ + 1) map (_ * 2)).force)
}