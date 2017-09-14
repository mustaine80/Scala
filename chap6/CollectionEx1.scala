//	Sequence, Set, Map Collection

object CollectionEx extends App
{
  //	List 는 앞부분에 원소를 빠르게 삭제하거나 추가할 수 있다.
  val colors = List("red", "blue", "green")
  println("List collection")
  println(colors.head)
  println(colors.tail)

  //	Array
  val fiveInts = new Array[Int](5)
  println("\n" + fiveInts)
  val fiveToOne = Array(1,2,3,4,5)
  println(fiveToOne)

  //	ListBuffer 앞뒤로 원소를 추가하는데 상수 시간이 걸린다.
  import scala.collection.mutable.ListBuffer
  val buf = ListBuffer(1,2,3)
  println("\n" + buf)
  buf += 4
  0 +=: buf
  println(buf)

  //	ArrayBuffer 는 원소를 추가/삭제하는데 평균적으로 상수 시간이 걸린다.
  import scala.collection.mutable.ArrayBuffer
  val arr = ArrayBuffer(1,2,3)
  println("\n" + arr)
  arr += 4
  0 +=: arr
  println(arr)

  //	String 클래스에는 exists 메소드가 없지만, 해당 메소드를 지원하는 StringOps 로의
  //	암시적 변환을 통해 문자열을 문자 시퀀스로 처리
  def hasUpperCase(s: String) = s.exists(_.isUpper)
  println("Rober Frost has a Uppercase : " + hasUpperCase("Rober Frost"))
  println("e e cumming has a Uppercase : " + hasUpperCase("e e cumming"))

  //	Set 은 특정 객체가 최대 하나만 들어가도록 보장한다.
  val text = "See Spot run. Run, Spot. Run!"
  println("\n" + text)
  val wordArray = text.split("[ !.,]+")
  println(wordArray)

  import scala.collection.mutable.Set
  val words = Set.empty[String]
  for (word <- wordArray)
    words += word.toLowerCase
  println(words)

  //	Map 은 컬렉션의 각 원소 값에 대해 연관 관계를 만든다.
  import scala.collection.mutable.Map
  val wordCounts = Map.empty[String, Int]
  for (word <- wordArray) {
    val lowerWord = word.toLowerCase
    val oldCount =
      if (wordCounts.contains(lowerWord))
        wordCounts(lowerWord)
      else
        0

    wordCounts += (lowerWord -> (oldCount + 1))
  }
  println("\n" + wordCounts)

  //	정렬된 집합과 맵을 구현하기 위해
  //	스칼라는 SortedSet, SortedMap trait 를 제공한다.
  import scala.collection.immutable.TreeSet
  val ts = TreeSet(9,3,1,8,0,2,7,4,6,5)
  println("\n" + ts)

  import scala.collection.immutable.TreeMap
  val tm = TreeMap(3->'x', 1->'x', 4->'x')
  println(tm)
}