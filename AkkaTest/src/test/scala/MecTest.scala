import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.nframework.mec.MEC_Proto.GetState
import com.nframework.mec._
import com.nframework.nom._
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.collection.mutable.ListBuffer

class MecTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  val dummy = NMessage("test", 0, Array[Byte](0))

  "MEC Actor" must {
    val props = MEC_Proto.props(testActor)
    val mec = system.actorOf(props, "c1")

    "update NOM data when it receives a message from MEB" +
      " and request MEB NOM Processing when it receives a message from User" in {
      mec ! DiscoverMsg(dummy)
      mec ! GetState(testActor)

      mec ! ReflectMsg(dummy)
      mec ! GetState(testActor)

      mec ! RecvMsg(dummy)
      mec ! GetState(testActor)

      mec ! RemoveMsg(dummy)
      mec ! GetState(testActor)

      expectMsgPF() {
        case ListBuffer(DiscoverMsg(msg)) => msg must be(dummy)
        case ListBuffer(ReflectMsg(msg)) => msg must be(dummy)
        case ListBuffer(RecvMsg(msg)) => msg must be(dummy)
        case ListBuffer(RemoveMsg(msg)) => msg must be(dummy)

        case _ => println("Dont' care. Msg is not defined in this test.")
      }
    }
  }

  //  todo: msgPop test

  //  todo:
}