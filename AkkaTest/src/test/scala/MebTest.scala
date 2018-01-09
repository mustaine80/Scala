import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.nframework.meb.MEB_Proto
import com.nframework.meb.MEB_Proto.{GetPubsSize, GetSubsSize}
import com.nframework.mec.MEC_Proto.{MebAttatch, MebDetatch, PubSubInfo, PubSubInfoForwarding}
import com.nframework.mec._
import com.nframework.nom.NMessage
import org.scalatest.{MustMatchers, WordSpecLike}

class MebTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  "MEB Actor 는 " must {
    val meb = system.actorOf(MEB_Proto.props(testActor), "mebDummy")

    "MEC 를 Publish/Subscribe 대상에 포함한다." in {
      meb ! MebAttatch("managerDummy1")
      expectMsg("MEB attatchment success")

      meb ! MebAttatch("managerDummy2")
      expectMsg("MEB attatchment success")
    }


    "Pub/Sub 메시지를 등록한다." in {
      val pubMsgDummy = PubSubInfo("msgDummy", "managerDummy1", "Publish")
      val subMsgDummy = PubSubInfo("msgDummy", "managerDummy2", "Subscribe")
      val pubSubMsgDummy = PubSubInfo("msgDummy", "managerDummy3", "PublishSubscribe")
      val neitherMsgDummy = PubSubInfo("msgDummy", "managerDummy3", "Neither")

      meb ! pubSubMsgDummy
      meb ! GetPubsSize(testActor); expectMsg(Map("msgDummy" -> Set("managerDummy3")))
      meb ! GetSubsSize(testActor); expectMsg(Map("msgDummy" -> Set("managerDummy3")))

      meb ! neitherMsgDummy
      meb ! GetPubsSize(testActor); expectMsg(Map.empty[String, Set[String]])
      meb ! GetSubsSize(testActor); expectMsg(Map.empty[String, Set[String]])

      meb ! pubMsgDummy
      meb ! GetPubsSize(testActor); expectMsg(Map("msgDummy" -> Set("managerDummy1")))
      meb ! GetSubsSize(testActor); expectMsg(Map.empty[String, Set[String]])

      meb ! subMsgDummy
      meb ! GetPubsSize(testActor); expectMsg(Map("msgDummy" -> Set("managerDummy1")))
      meb ! GetSubsSize(testActor); expectMsg(Map("msgDummy" -> Set("managerDummy2")))
    }


    "Pub/Sub 메시지 등록 완료 여부를 알린다." in {
      meb ! PubSubInfoForwarding("managerDummy1")
      expectMsg("PubSub info forwarding complete")

      meb ! PubSubInfoForwarding("managerDummy2")
      expectMsg("PubSub info forwarding complete")
    }


    "수신한 Pub 메시지에 대응하는 Sub 메시지를 Subscribers 에게 전송한다." in {
      val dummy = NMessage("msgDummy", 1, Array[Byte](0))

      meb ! RegisterMsg(dummy)
      expectMsg(DiscoverMsg(dummy))

      meb ! UpdateMsg(dummy)
      expectMsg(ReflectMsg(dummy))

      meb ! SendMsg(dummy)
      expectMsg(RecvMsg(dummy))

      meb ! DeleteMsg(dummy)
      expectMsg(RemoveMsg(dummy))
    }


    "MEC 를 Publish/Subscribe 대상에서 제외한다." in {
      meb ! MebDetatch("managerDummy1")
      expectMsg("MEB detatchment success")

      meb ! MebDetatch("managerDummy2")
      expectMsg("MEB detatchment success")
    }

  }
}