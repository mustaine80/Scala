object CollectionEx2 extends App {
  //  변경 불가능한 컬렉션은 += 연산자를 지원하지 않지만
  //  var 타입에 할당할 경우 '='로 끝나는 연산을 사용해 people 을 변경할 수 있다.(신규 컬렉션 재할당)
  var people = Set("Nancy", "Jane")
  people += "Bob"
  people ++= List("Tom", "Harry")
  people -= "Jane"
  println(people)

  //  컬렉션 초기화
  //  Any 타입의 원소를 원한다면 타입 파라미터를 명시해야 한다.
  import scala.collection.mutable
  val stuff = mutable.Set[Any](42)  /// Set(42)로 할경우 Set[Int](42)가 된다.
  stuff += "abracatabra"
  println(stuff)

  //  컬렉션을 다른 컬렉션으로부터 초기화
  val colors = List("blue", "yellow", "red", "green")
  import scala.collection.immutable.TreeSet
  val treeSet = TreeSet[String]() ++ colors
  println(treeSet)

  //  배열이나 리스트로 바꾸기
  println(treeSet.toList)
  println(treeSet.toArray.mkString(" ,"))

  //  mutable -> immutable, immutable -> mutable
  val mutableSet = mutable.Set.empty ++= treeSet
  mutableSet += "black"
  println(mutableSet)
  val immutableSet = Set.empty ++ mutableSet
  println(immutableSet)

  def longestWord(words: Array[String]): (String, Int) = {
    var word = words(0)
    var idx = 0
    for (i <- 1 until words.length) {
      if (words(i).length > word.length) {
        word = words(i)
        idx = i
      }
    }
    (word, idx)
  }

  //  Tuple
  val longest = longestWord("The quick brown fox".split(" "))
  println("longest word : " + longest._1 + ", index : " + longest._2)
  val (word, idx) = longest
  println("longest word : " + word + ", index : " + idx)
}