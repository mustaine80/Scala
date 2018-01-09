package com.nframework.mec

import akka.actor.{Actor, ActorRef, Props, Timers}
import com.nframework.mec.MEC_Proto._
import com.nframework.nom._

import scala.concurrent.duration._


object MEC_Proto {
  private object Pop
  private object TickKey

  /*  MEB actor 에게 MEC actor reference 를 전달한다.
   *  MEB actor 는 MebAttatch 메시지 수신 시 sender reference 를 이용하여 MEC reference 를 획득한다.
   *  또한, 전달인자를 이용하여 User Manager 를 pub/sub table 에 등록할 수 있다. */
  case class MebAttatch(name: String)
  case class MebDetatch(name: String)

  case class PubSubInfoForwarding(msg: String) /// msg sharing 정보 전파 요청이 완료된 것을 확인하기 위한 용도

  /* pub/sub 정보를 MEB 에 넘기기 위한 클래스 */
  case class PubSubInfo(msgName: String, managerName: String, sharing: String)

  def props(test: ActorRef) = {
    Props(new MEC_Proto("mecTestActor", test, test))
  }

  /*  test 전용이며, testActor 에 MEC actor 상태를 전송하기 위해 사용한다. */
  case class GetSubMsgLists(test: ActorRef)
}


/// msg: mec -> meb
abstract class PubMsg

case class RegisterMsg(msg: NMessage) extends PubMsg
case class UpdateMsg(msg: NMessage) extends PubMsg
case class SendMsg(msg: NMessage) extends PubMsg
case class DeleteMsg(msg: NMessage) extends PubMsg


/// msg: meb -> mec
abstract class SubMsg

case class DiscoverMsg(msg: NMessage) extends SubMsg
case class ReflectMsg(msg: NMessage) extends SubMsg
case class RecvMsg(msg: NMessage) extends SubMsg
case class RemoveMsg(msg: NMessage) extends SubMsg


class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef)
  extends Actor with Timers {
  var subMsgList = List.empty[SubMsg]

  init()

  def init(): Unit = {
    meb ! MebAttatch(userName)
    timers.startPeriodicTimer(TickKey, Pop, 1.millisecond)
    println("MEC initialize ...")
  }

  //  Pub/Sub 특성 상 stack 처럼 처리해도 상관없다.
  def msgPop(): Unit = {
    subMsgList.foreach(user ! _)
    subMsgList = List.empty[SubMsg]
  }

  def pubSubInfoForwarding: Unit = {
    if (Proto_NOMParser.parse("src/main/Resources/test.json")) {
      Proto_NOMParser.objectTypes.foreach{ case (msgName, obj) => meb ! PubSubInfo(msgName, userName, obj.sharing) }
      Proto_NOMParser.interactionTypes.foreach{ case (msgName, param) => meb ! PubSubInfo(msgName, userName, param.sharing) }
      meb ! PubSubInfoForwarding(userName)
    } else {
      println("[MEC] NOM parsing fail!")
    }
  }


  def receive = {
    //  user -> mec: data request (simple forwarder)
    case m: PubMsg => meb ! m

    //  meb -> mec: data push (simple Stack)
    case m: SubMsg => subMsgList = m :: subMsgList

    //  MEB pub sub forwarding ack 처리
    case "PubSub info forwarding complete" =>
      println("PubSub info forwarding success")
      user ! PubSubInfoForwarding(userName)

    //  MEB attach ack 처리
    case "MEB attatchment success" =>
      println("[MEC] MEB attatchment success")
      pubSubInfoForwarding

    //  MEC 종료 시 MEB detach ack 처리
    //  todo: 사용자 요청이 아니라 MEC 종료 시 자동으로 수행되어야 하는 작업이 되도록 보완해야 한다.
    case "MEC quit request" =>
      println("[MEC] user request stop MEC")
      meb ! MebDetatch(userName)

    case "MEB detatchment success" =>
      println("[MEC] MEB detatchment success")

    //  MEC self pub/sub msg pop schedule
    case Pop => msgPop()

    //  다중 JVM 환경에서 actor test 를 위한 코드
    case GetSubMsgLists(test) =>
      test ! subMsgList

    // unknown message
    case m: String => println(s"received $m")
  }
}