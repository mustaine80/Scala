object Foo {
  def seqToString[T](seq: Seq[T]): String = seq match {
    case head +: tail => s"$head +:" +seqToString(tail)
    case Nil => "Nil"
  } 
}

abstract class Item
case class Article(description: String, price: Double) extends Item
case class Bundle(description: String, discount: Double, items: Item*) extends Item {
  def price(it: Item): Double = it match {
    case Article(_, p) => p
    case Bundle(_, disc, its @ _*) => 
      its.map(price _).sum - disc
  }
}

object CaseClassEx extends App {
  val bar = Bundle("Father's day special", 20.0,
              Article("scala for the impatient", 39.95),
              Bundle("Anchor Distrillery Sampler", 10.0,
                Article("Old Potrero Straight Rye Whiskey", 79.95),
                Article("Junipero Gin", 32.95)
        )
      )

  bar match {
    // case Bundle(_, _, Article(descr, _), _*) => println("Bundle Matching")
    case Bundle(_, _, art @ Article(_, _), rest @ _*) => println(art.description + " : " + art.price)
    case _ => println("nothing match")
  }

  println("\n# 시퀀스에 일치시키기")

  val nonEmptySeq = Seq(1, 2, 3, 4, 5)
  val emptySeq = Seq.empty[Int]
  val nonEmptyList = List(1, 2, 3, 4, 5)
  val emptyList = Nil
  val nonEmptyVector = Vector(1, 2, 3, 4, 5)
  val emptyVector = Vector.empty[Int]
  val nonEmptyMap = Map("one" -> 1, "two" -> 2, "three" -> 3)
  val emptyMap = Map.empty[String, Int]

  for (seq <- Seq(
    nonEmptySeq, emptySeq, nonEmptyList, emptyList, nonEmptyVector, emptyVector, nonEmptyMap.toSeq, emptyMap.toSeq)) {
    println(Foo.seqToString(seq))
  }
}