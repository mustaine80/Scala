object PatternMatchTest2 extends App {
  
  // Simple example
  val bools = Seq(true, false)
  
  for(bool <- bools) {
    bool match {
      case true => println("Got heads")
      case flase => println("Got tails")
    }
  }
  
  // value, variable, type match
  for ( x <- Seq(1, 2, 2.7, "one", "two", 'four) ) {
    val str = x match {
      case 1          => "int 1"
      case i: Int     => "other int: " + i
      case d: Double  => "a double: " + d
      case "one"      => "string one"
      case s: String  => "other string: " + s
      case unexpected  => "unexpected value: " + unexpected  // unexpected is not a keyword
    }
    
    println(str)
  }
  
  // revised example of above one
  for (x <- Seq(1, 2, 2.7, "one", "two", 'four) ) {
    val str = x match {
      case 1                    => "int 1"
      case _: Int | _: Double   => "a number: " + x
      case "one"                => "string one"
      case _: String            => "other string: " + x
      case _                    => "unexpected value: " + x
    }      
  
    println(str)
  }
    
  // error!!
  def checkYError(y: Int) = {
    for( x <- Seq(99, 100, 101) ) {
      val str = x match {
        case y => "found y! : " + y
        case i: Int => "int: " + i
      }
      
      println(str)
    }
  }
  
  checkYError(100)
  
  def checkY(y: Int) = {
    for( x <- Seq(99, 100, 101) ) {
      val str = x match {
        case `y` => "found y! : " + y
        case i: Int => "int: " + i
      }
      
      println(str)
    }
  }
  
  checkY(100)
}

object PatternMachingTest2Seq extends App {
  val nonEmptySeq     = Seq(1, 2, 3, 4, 5)
  val emptySeq        = Seq.empty[Int]
  val nonEmptyList    = List(1, 2, 3, 4, 5)
  val emptyList       = List.empty[Int]
  val nonEmptyVector  = Vector(1, 2, 3, 4, 5)
  val emptyVector     = Vector.empty[Int]
  val nonEmptyMap     = Map("one" -> 1, "two" -> 2, "three" -> 3)
  val emptyMap        = Map.empty[String, Int]
  
  def seqToString[T](seq: Seq[T]): String = seq match {
    case head +: tail => s"($head +: ${seqToString(tail)})"
    case Nil => "Nil"
  }
  
  for(seq <- Seq(
      nonEmptySeq, emptySeq, nonEmptyList, emptyList, nonEmptyVector, emptyVector, nonEmptyMap.toSeq, emptyMap.toSeq)) {
    println(seqToString(seq))
  }
}