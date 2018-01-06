import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.nframework.mec.MEC_Proto._
import com.nframework.mec._
import com.nframework.nom._
import scala.concurrent.duration._
import org.scalatest.{MustMatchers, WordSpecLike}

class MecTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike   /// DSL. BDD style test coding support
  with MustMatchers   /// easy reading assertion support
  with StopSystemAfterAll {

  "MEC Actor 기능 요구사항은 " must {
    val mec = system.actorOf(MEC_Proto.props(testActor), "mec")
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
      List(DiscoverMsg(dummy), ReflectMsg(dummy), RecvMsg(dummy), RemoveMsg(dummy)).foreach(mec ! _)
      mec ! GetSubMsgLists(testActor)
      expectMsg(List(RemoveMsg(dummy), RecvMsg(dummy), ReflectMsg(dummy), DiscoverMsg(dummy)))
    }

    "MEB 에 자신을 Publish/Subscribe 대상으로부터 제외할 것을 요청한다." in {
      mec ! "MEC quit request"

      expectMsgPF() {   /// msgPop 이 수행되어 UserManager(testActor ref)로 전송되는 메시지가 수신될 수 있기 때문에 PF 를 사용한다.
        case MebDetatch(userName) => userName must be("mecTestActor")
        case _ =>
      }
    }


    //  todo: 프레임워크에서 보장할 수 있는 요구사항이 아니지 않은가?
    "1 ms 주기로 저장된 Subscribe message 를 User Manager 에 전달한다." in {
      for (i <- 1 to 100)
        mec ! ReflectMsg(NMessage("dummy message for Pub/Sub test", i, Array[Byte](0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))

      within(1.millisecond) {
        mec ! RunMsgPop(testActor)

        expectMsgPF() {
          case m: Int => m must be(0)
          case _ =>
        }
      }
    }

  }
}