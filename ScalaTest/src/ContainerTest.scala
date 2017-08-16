import scala.collection.mutable.Map
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object ContainerTest {
  def main(args: Array[String]) {
    val myList = List("Engliand" -> "London", "Germany" -> "Berlin")  // list of tuples
    val myMap = myList.groupBy(_._1).map(e=>(e._1, e._2(0)._2))
    val betterConversion = Map(myList:_*)
    val scala28Map = myList.toMap
    
    val myList2 = List("England", "London", "Germany", "Berlin")
    val m = myList2.map( s=> (s, s.length) ).toMap
    val m2 = myList2.map( s=> (s.length, s) ).toMap
    
    println(m)
    println(m2)
    
    val ndx = List(1, 2)
    val vals = List("One", "Two")
    val mymap = for( (i, v) <- ndx.zip(vals) ) yield { i -> v }
    val mm = ndx.zip(vals)
    
    println(mymap)
    println(mm)
    
    val listBuffer = ListBuffer(1, 2, 3)
    val listBuffer2 = listBuffer :+ 4
    val arrayBuffer = ArrayBuffer(1, 2)
    val arrayBuffer2 = arrayBuffer :+ 3
    
    println(listBuffer)
    println(listBuffer2)
    println(arrayBuffer)
    println(arrayBuffer2)
    
    val a = Array(2, 3, 5, 7, 11)
    val result = for(elem <- a) yield 2 * elem
    result.foreach(println)
  }
}
