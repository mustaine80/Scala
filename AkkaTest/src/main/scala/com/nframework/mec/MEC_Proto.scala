package com.nframework.mec

import akka.actor.{Actor, ActorRef, Props, Timers}
import com.nframework.mec.MEC_Proto._
import com.nframework.nom._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._


object MEC_Proto {
  private object Pop
  private object TickKey

  /*  MEB actor 에게 MEC actor reference 를 전달한다.
   *  MEB actor 는 MebAttatch 메시지 수신 시 sender reference 를 이용하여 MEC reference 를 획득한다.
   *  또한, 전달인자를 이용하여 User Manager 를 pub/sub table 에 등록할 수 있다. */
  case class MebAttatch(name: String)
  case class MebDetatch(name: String)

  case class PubSubInfoForwarding(name: String) /// msg sharing 정보 전파 요청이 완료된 것을 확인하기 위한 용도

  /* pub/sub 정보를 MEB 에 넘기기 위한 클래스 */
  case class PubSubInfo(msgName: String, managerName: String, sharing: String)

  def props(test: ActorRef) = {
    Props(new MEC_Proto("MecTestActor", null, test))
  }

  /*  test 전용이며, testActor 에 MEC actor 상태를 전송하기 위해 사용한다. */
  case class GetState(test: ActorRef)
}


abstract class PubSub

/// msg: mec -> meb
case class RegisterMsg(msgName: String, objID: Int, userName: String) extends PubSub
case class UpdateMsg(msg: NMessage) extends PubSub
case class SendMsg(msg: NMessage) extends PubSub
case class DeleteMsg(msg: NMessage) extends PubSub


/// msg: meb -> mec
case class DiscoverMsg(msg: NMessage) extends PubSub
case class ReflectMsg(msg: NMessage) extends PubSub
case class RecvMsg(msg: NMessage) extends PubSub
case class RemoveMsg(msg: NMessage) extends PubSub


class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef)
  extends Actor with Timers {

  var discoveredNOMList = ListBuffer[DiscoverMsg]()
  var reflectedNOMList = ListBuffer[(ReflectMsg)]()
  var receivedNOMList = ListBuffer[RecvMsg]()
  var removedNOMList = ListBuffer[RemoveMsg]()

  init()

  def init(): Unit = {
    meb ! MebAttatch(userName)
    timers.startPeriodicTimer(TickKey, Pop, 1.millisecond)
    println("MEC initialize ...")
  }

  def msgPop[U <: PubSub](xs: ListBuffer[U], proc: U => Unit): ListBuffer[U] = {
    xs match {
      case ListBuffer() => xs
      case x +: xsLeft =>
        proc(x)
        msgPop(xsLeft, proc)
    }
  }

  def task(): Unit = {
    discoveredNOMList = msgPop(discoveredNOMList, user ! _)
    reflectedNOMList = msgPop(reflectedNOMList, user ! _)
    receivedNOMList = msgPop(receivedNOMList, user ! _)
    removedNOMList = msgPop(removedNOMList, user ! _)
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
    //  user -> mec: data request
    case m: RegisterMsg => meb ! m
    case m: UpdateMsg => meb ! m
    case m: SendMsg => meb ! m
    case m: DeleteMsg => meb ! m

    //  meb -> mec: data push
    //  mec 는 단순히 Pub/Sub msg 에 대한 stack 역할만을 수행한다.
    case m: DiscoverMsg => discoveredNOMList += m
    case m: ReflectMsg => reflectedNOMList += m
    case m: RecvMsg => receivedNOMList += m
    case m: RemoveMsg => removedNOMList += m

    //  MEB attach ack 처리
    case "MEB attatchment success" =>
      println("[MEC] MEB attatchment success")
      pubSubInfoForwarding

    //  MEC 종료 시 MEB detach ack 처리
    case "MEC quit request" =>
      println("[MEC] user request stop MEC")
      meb ! MebDetatch(userName)

    case "MEB detatchment success" =>
      println("[MEC] MEB detatchment success")


    //  MEB pub sub forwarding ack 처리
    case "PubSub info forwarding complete" =>
      println("PubSub info forwarding success")
      user ! PubSubInfoForwarding(userName)

    //  MEC self pub/sub msg pop schedule
    case Pop => task()

    //  다중 JVM 환경에서 actor test 를 위한 코드
    case GetState(test) => {
      test ! discoveredNOMList
      test ! reflectedNOMList
      test ! receivedNOMList
      test ! removedNOMList
    }

    // unknown message
    case m: String => println(s"received $m")
  }
}