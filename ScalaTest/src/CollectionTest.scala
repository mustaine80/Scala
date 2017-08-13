import scala.collection.immutable.Map
import scala.collection.immutable.List
import scala.util.Sorting.stableSort
import scala.util.Sorting.quickSort

object CollectionTest {
  def main(args: Array[String]) {
    var l = List("Berlin", "New York", "Seoul", "Tokyo", "Beijing")
    var tl = List( (1, "Berlin"), (2, "New York"), (3,"Seoul"), (4, "Tokyo"), (5, "Beijing") )
    var a = Array(1, 2, 7, 10, 3)
    
    val tp = (1, 2, 3, 4, 5)
    println("_2: " + tp._2)
    
    var m = l.map( s => (s, s.length) ).toMap
    var tm = tl.toMap
    
    var newmap = (for(elem <- l) yield (elem, elem.length) ).toMap
    var newmap2 = l.map( s => (s, s.length) ).toMap
    println(newmap2)
    
    println(tm)
    //println(m)
   
    println("Length of seoul: " + m.getOrElse("Seoul", -1))
    
    var ol = stableSort(l).toList
    println(ol)
    
    println( ol.mkString("<", ", ", ">") )
    
    TestObject.testFunc
    TestObject.testFunc    
    
    val to = TestObject()
    to.d = 2
    println(to.d)
    
    val time = new Time
    time.hour = 12
    println(time.hour)
  }
}

class TestObject(a : Int, var b:Int) {
  
  val c = 0.0
  
  def this(a: Int) {
    this(0, 0)
  }
  
  def d = b
  def d_=(n: Int) {
    if(n > 10) b = n
  }
}

object TestObject {
  println("This is test object")
  
  def testFunc() {
    println("This is test method")
  }
  def apply() = new TestObject(0, 0)
  
  println("alsdkjfsldkf")
  testFunc
}


class Time {
  private[this] var h = 12
  private[this] var m = 0
  
  var hh = 0
  
  def hour: Int = h
  def hour_= (x: Int) {
    require(0 <= x && x < 24)
    h = x
  }
  
  def minute = m
  def minute_= (x: Int) {
    require(0 <= x && x < 60)
    m = x
  }
}