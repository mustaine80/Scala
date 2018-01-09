import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.nframework.mec.MEC_Proto._
import com.nframework.mec._
import com.nframework.nom._
import org.scalatest.{MustMatchers, WordSpecLike}

class MecTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  "MEC Actor 는 " must {
    val mec = system.actorOf(MEC_Proto.props(testActor), "mecDummy")
    val dummy = NMessage("dummy message for Pub/Sub test", 1, Array[Byte](0))

    "MEB 에 자신을 Publish/Subscribe 대상에 포함할 것을 요청한다." in {
      expectMsg(MebAttatch("mecTestActor"))
    }


    "MEB 에 자신의 Publish/Subscribe message sharing 정보를 전송한다." in {
      mec ! "MEB attatchment success"

      val recv = receiveWhile() {
        case PubSubInfo(_, userName, _) => userName
        case PubSubInfoForwarding(userName) => userName
      }.toSet

      recv must be(Set("mecTestActor"))
    }


    "UserManager 에 Publish/Subscribe message sharing 정보 전달 완료 여부를 전송한다." in {
      mec ! "PubSub info forwarding complete"

      expectMsg(PubSubInfoForwarding("mecTestActor"))
    }


    "User Manager 로부터 Publish message 를 수신하여 MEB 에 전달한다." in {
      mec ! RegisterMsg(dummy)
      expectMsg(RegisterMsg(dummy))

      mec ! UpdateMsg(dummy)
      expectMsg(UpdateMsg(dummy))

      mec ! SendMsg(dummy)
      expectMsg(SendMsg(dummy))

      mec ! DeleteMsg(dummy)
      expectMsg(DeleteMsg(dummy))
    }


    "MEB 로부터 Subscribe message 를 수신하여 저장한다." in {
      mec ! DiscoverMsg(dummy)
      expectMsg(DiscoverMsg(dummy))

      mec ! ReflectMsg(dummy)
      expectMsg(ReflectMsg(dummy))

      mec ! RecvMsg(dummy)
      expectMsg(RecvMsg(dummy))

      mec ! RemoveMsg(dummy)
      expectMsg(RemoveMsg(dummy))
    }

    "MEB 에 자신을 Publish/Subscribe 대상으로부터 제외할 것을 요청한다." in {
      mec ! "MEC quit request"
      expectMsg(MebDetatch("mecTestActor"))
    }
  }
}