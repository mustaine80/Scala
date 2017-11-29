import akka.actor.{ActorSystem, Props}
import akka.testkit.TestKit
import com.nframework.mec.MEC_Proto.GetState
import com.nframework.mec._
import com.nframework.nom.{NChar, NOM, NValueType}
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.collection.mutable.ListBuffer

//  todo: just only use MEC's receive test... need to implement NOM
case class DummyNOM(objName: String, value: NValueType) extends NOM

class MebPushedDataApplyTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  "MEC Actor" must {
    val mec = system.actorOf(Props(new MEC_Proto("MEC", null, null)), "m1")
    val dummy = DummyNOM("NChar", NChar('a'))

    "discover NOM list update when it receives a message from MEB" in {
      mec ! DiscoverMsg(dummy)
      mec ! GetState(testActor)
      expectMsg(ListBuffer(DiscoverMsg(dummy)))
    }

    "reflect NOM list update when it receives a message from MEB" in {
      mec ! ReflectMsg(dummy, 0x5)
      mec ! GetState(testActor)
      expectMsg(ListBuffer(ReflectMsg(dummy, 0x5)))
    }

    "received NOM list update when it receives a message from MEB" in {
      mec ! RecvMsg(dummy)
      mec ! GetState(testActor)
      expectMsg(ListBuffer(RecvMsg(dummy)))
    }

    //  todo: if we remove msg then discoveredNOMList test result changed...
//    "remove NOM list update when it receives a message from MEB" in {
//      mec ! RemoveMsg(dummy)
//      mec ! GetState(testActor)
//      expectMsg(ListBuffer(RemoveMsg(dummy)))
//    }
  }
}