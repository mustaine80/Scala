import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

class MEB_Dummy {

}

object MEB_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("client")
    val meb = ActorSystem("mebDummy", config)

    val path = "akka.tcp://server@0.0.0.0:2551/user/mec"  //  todo: need to distribute remotely
    val mec = meb.actorSelection(path)

    mec ! "Hello Remote World!"
  }
}
