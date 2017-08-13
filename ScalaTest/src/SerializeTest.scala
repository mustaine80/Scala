import java.io._
import scala.collection.mutable._

@SerialVersionUID(42L) class NewPerson(val id:Int, val name:String, val age: Int) extends Serializable {
  private val friends = new ArrayBuffer[NewPerson]
  def addFriend(f: NewPerson) {
    friends += f
  }
  def getFriends() : Array[NewPerson] = {
    friends.toArray
  }
}

object SerializeTest extends App {
  val fred = new NewPerson(1, "Fred", 37)
  val john = new NewPerson(2, "John", 38)
  val james = new NewPerson(3, "James", 39)
  
  fred.addFriend(john)
  fred.addFriend(james)
  
  val out = new ObjectOutputStream(new FileOutputStream("/tmp/test.obj"))
  out.writeObject(fred)
  out.close()
  
  val in = new ObjectInputStream(new FileInputStream("/tmp/test.obj"))
  val savedFred = in.readObject().asInstanceOf[NewPerson]
  
  println(savedFred.name)
  
  savedFred.getFriends().map(f => f.name + "/" +f.age).foreach(println)
}