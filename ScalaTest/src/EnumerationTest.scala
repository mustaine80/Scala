object TrafficLightColor extends Enumeration {
  //val RED, YELLOW, GREEN = Value
  val RED = Value(1)
  val YELLOW = Value
  val GREEN = Value
}

object EnumerationTest extends App {
  val red = TrafficLightColor.RED
  val yellow = TrafficLightColor.YELLOW
  val green = TrafficLightColor.GREEN
  
  println(red.id)
  println(yellow.id)
  println(green.id)
}