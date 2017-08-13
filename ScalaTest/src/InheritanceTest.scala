abstract class Person(val name: String, val age: Int) {
  val i: Int
  def id: Int
}

class Employee(name: String, age: Int, val salary: Double) extends Person(name, age) {
  //override def id = name.hashCode()
  def id = name.hashCode()    // abstract method는 override keyword가 없어도 됨
  val i = 0  // val override
}


object InheritanceTest extends App {
  val e = new Employee("John", 37, 5000)
  val p: Person = e
  
  println(e.salary)
  println(p.name)
  
  val fred = new Person("James", 38) {
    val i = 1
    val id = 1234
  }
  
  println(fred.id)
  
  printAny("Hello")
  printUnit("Hello")
  
  def printAny(x: Any) {
    println(x)
  }
  def printUnit(x: Unit) {
    println(x)
  }
}