import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.nframework.mec.MEC_Proto.GetState
import com.nframework.mec._
import com.nframework.nom._
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.collection.mutable.ListBuffer

//  todo: just only use MEC's receive test... need to implement NOM
class MecTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  val dummy = DummyNOM("NChar", NChar_Dummy('a'))

  "MEC Actor" must {
    val props = MEC_Proto.props(testActor)
    val mec = system.actorOf(props, "c1")

    //  todo: need to extract 'request' test. but, expectMsgPF() routine have a unknown error.
    "update NOM data when it receives a message from MEB" +
      " and request MEB NOM Processing when it receives a message from User" in {
      mec ! DiscoverMsg(dummy)
      mec ! GetState(testActor)

      mec ! ReflectMsg(dummy, Array[Byte](5))
      mec ! GetState(testActor)

      mec ! RecvMsg(dummy)
      mec ! GetState(testActor)

      mec ! RemoveMsg(dummy)
      mec ! GetState(testActor)

      mec ! RegisterMsg("Fire", "SimMgr")
      mec ! UpdateMsg(dummy)
      mec ! SendMsg(dummy)
      mec ! DeleteMsg(dummy)

      expectMsgPF() {
        case ListBuffer(DiscoverMsg(msg)) => msg must be(dummy)
        case ListBuffer(ReflectMsg(msg, buf)) =>
          msg must be(dummy)
          buf must be(0x5)
        case ListBuffer(RecvMsg(msg)) => msg must be(dummy)
        case ListBuffer(RemoveMsg(msg)) => msg must be(dummy)

        case RegisterMsg(msgName, userName) =>
          msgName must be("Fire")
          userName must be("SimMgr")
        case UpdateMsg(msg: NOM) => msg must be(dummy)
        case SendMsg(msg: NOM) => msg must be(dummy)
        case DeleteMsg(msg: NOM)=> msg must be(dummy)

        case _ => println("Dont' care. Msg is not defined in this test.")
      }
    }
  }
}