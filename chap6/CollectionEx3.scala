object collectionEx extends App {
	//	Traversable trait
	//	맵
	val xs = List(1,2,3,4,5)
	println("Traversable trait 연산자 예")
	
	val unitFn = (i: Int) => print(s"${i * 2}, ")
	xs foreach unitFn
	println()

	val ys = List(6,7,8,9,10)
	println(xs ++ ys)

	val selfAddFn = (i: Int) => i+ i
	println(xs map selfAddFn)

	val colRetFn = (i: Int) => i :: Nil
	println(xs flatMap colRetFn)

	val partialFn:PartialFunction[Int, String] = {
		case 1 => "one"
		case 2 => "two"
	}
	println(xs collect partialFn)

	//	변환
	println(xs.toArray.mkString(" ,"))
	println(xs.toList)

	val it = xs.toIterable.iterator
	println(it.next())

	println(xs.toSeq)
	println(xs.toIndexedSeq)
	println(xs.toStream)
	println(xs.toSet)

	val z = Seq((1, "one"), (2, "two"), (3, "three"))
	println(z.toMap)

	//	복사
	import scala.collection.mutable.Buffer
	val buf = Buffer[Int]()
	xs copyToBuffer buf
	println(buf)

	val arr = new Array[Int](3)
	xs copyToArray arr
	print(arr.mkString("" ," ,", "\n"))

	//	크기 정보
	println(s"xs is empty : ${xs.isEmpty}")
	println(s"xs is nonEmpty : ${xs.nonEmpty}")
	println(s"xs size : ${xs.size}")
	println(s"xs is definite : ${xs.hasDefiniteSize}")

	//	원소 가져오기
	println(s"xs's head : ${xs.head}")
	println(s"xs's head(optional) : ${xs.headOption}")
	println(s"xs's last : ${xs.last}")
	println(s"xs's last(optional) : ${xs.lastOption}")
	println(s"first element greater than 3 in xs : ${xs find (x => x > 3)}")

	//	하위 컬렉션
	println(s"xs.tail : ${xs.tail}")	//	head 제외한 나머지
	println(s"xs.init : ${xs.init}")	//	last 제외한 나머지
	println(s"xs slice (1,3) : ${xs slice (1,3)}")
	println(s"xs take 3 : ${xs take 3}")
	println(s"xs drop 2 : ${xs drop 2}")
	println(s"xs takeWhile x < 3 : ${xs takeWhile (x => x < 3)}")
	println(s"xs dropWhile x < 3 : ${xs dropWhile (x => x < 3)}")
	println(s"xs filter evenNum : ${xs filter (x => x % 2 == 0)}")
	println(s"xs filterNot evenNum : ${xs filterNot (x => x % 2 == 0)}")

	val lazyFilter = xs withFilter (x => x % 2 == 0)
	println(s"xs withFilter evenNum : $lazyFilter")
	println(s"withFilter apply map : ${lazyFilter map selfAddFn}")

	//	분할
	///	take, drop pair return
	println(s"xs splitAt 3 : ${xs splitAt 3}")
	
	/// takeWhile, dropWhile pair return
	println(s"xs span evenNum : ${xs span (x => x % 2 == 0)}")

	///	filter, filterNot pair return
	println(s"xs partition evenNum : ${xs partition (x => x % 2 == 0)}")

	///	구분 함수 f 에 원소를 넘겼을 때의 결과 값에 따라 분할. 컬렉션 맵 반환
	println(s"xs groupBy evenNum : ${xs groupBy (x => x % 2 == 0)}")

	//	원소 조건
	println(s"xs forall x < 6 : ${xs forall (x => x < 6)}")
	println(s"xs exists x > 6 : ${xs exists (x => x > 6)}")
	println(s"xs count x > 4 : ${xs count (x => x > 4)}")

	//	폴드
	println(s"(2 /: xs)(_*2 - _) : ${(2 /: xs)(_*2 - _)}")
	println(s"(xs :\\ 2)(_*2 - _) : ${(xs :\ 2)(_*2 - _)}")
	println(s"xs.foldLeft(2)(_*2 - _) : ${xs.foldLeft(2)(_*2 - _)}")
	println(s"xs.foldRight(2)(_*2 - _) : ${xs.foldRight(2)(_*2 - _)}")

	///	TODO: reduceLeft 가 좀 더 generic 한 folderLeft 라고 하는데, 사용처를 잘 모르겠음
	println(s"xs reduceLeft (_*2 - _) : ${xs reduceLeft (_*2 - _)}")
	println(s"xs reduceRight (_*2 - _) : ${xs reduceRight (_*2 - _)}")

	//	특정 폴드
	println(s"xs sum : ${xs sum}")
	println(s"xs product : ${xs product}")
	println(s"xs min : ${xs min}")
	println(s"xs max : ${xs max}")

	//	문자열
	val b = StringBuilder.newBuilder
	b.append("Hello, World. ")
	println(s"xs addString b : ${xs addString (b, "<<", " ,", ">>")}")
	println(s"xs mkString : ${xs mkString ("<<", " ,", ">>")}")
	println(s"xs stringPrefix : ${xs stringPrefix}")

	//	뷰
	//	view 는 lazy collection 을 만든다.
	val lazyView = (0 to 1000000000).view.filter(_ % 2 == 0).take(10).toList
	println(s"collection view filter : $lazyView")
	val lazyViewFromTo = (0 to 1000000000).view(4, 12).filter(_ % 2 == 0).take(10).toList
	println(s"collection view(from, to) filter : $lazyViewFromTo")
}