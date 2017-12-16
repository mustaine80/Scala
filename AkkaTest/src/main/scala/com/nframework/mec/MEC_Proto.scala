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
case class RegisterMsg(msgName: String, userName: String) extends PubSub
case class UpdateMsg(msg: List[NOM]) extends PubSub
case class SendMsg(msg: List[NOM]) extends PubSub
case class DeleteMsg(msg: List[NOM]) extends PubSub


/// msg: meb -> mec
case class DiscoverMsg(msg: List[NOM]) extends PubSub
case class ReflectMsg(msg: List[NOM]) extends PubSub
case class RecvMsg(msg: List[NOM]) extends PubSub
case class RemoveMsg(msg: List[NOM]) extends PubSub


class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef)
  extends Actor with Timers {

  var receivedNOMList = ListBuffer[RecvMsg]()
  var reflectedNOMList = ListBuffer[(ReflectMsg)]()
  var discoveredNOMList = ListBuffer[DiscoverMsg]()
  var removedNOMList = ListBuffer[RemoveMsg]()

  init()

  def init(): Unit = {
    meb ! MebAttatch(userName)
    timers.startPeriodicTimer(TickKey, Pop, 1.second) //  todo: 주기를 1ms 으로 변경해야 한다.
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
    println("NOM msg processing...")

    discoveredNOMList = msgPop(discoveredNOMList, user ! _)
    reflectedNOMList = msgPop(reflectedNOMList, user ! _)
    receivedNOMList = msgPop(receivedNOMList, user ! _)
    removedNOMList = msgPop(removedNOMList, user ! _)
  }


  //  todo: Seq 를 한번에 넘겨야 하는데, 직렬화 처리에 문제가 있다. 일단 elem 단위로 넘긴다.
  //  todo: parsing 결과에 대해서도 future 처리가 필요하다.
  def pubSubInfoForwarding: Unit = {
    Proto_NOMParser.parse("src/main/Resources/test.json")
    Proto_NOMParser.objectTypes.foreach{ case (msgName, obj) => meb ! PubSubInfo(msgName, userName, obj.sharing) }
    Proto_NOMParser.interactionTypes.foreach{ case (msgName, param) => meb ! PubSubInfo(msgName, userName, param.sharing) }
  }


  def receive = {
    //  user -> mec: data request
    case m: RegisterMsg => meb ! m
    case m: UpdateMsg => meb ! m
    case m: SendMsg => meb ! m
    case m: DeleteMsg => meb ! m

    //  meb -> mec: data push
    case m: DiscoverMsg => discoveredNOMList += m
    case m: ReflectMsg => reflectedNOMList += m
    case m: RecvMsg => {
      //  todo: need to implement
      //  val msg = nomMsg.clone()
      //  msg.setOwner(userName)
      receivedNOMList += m
    }

    case s @ RemoveMsg(msg) => {
      val r = (discoveredNOMList find (_.msg == s.msg)).get
      discoveredNOMList -= r
      removedNOMList += RemoveMsg(r.msg)
    }

    //  MEB attach ack 처리
    case "MEB attatchment success" =>
      println("'Simulation Manager - MEB' attatchment success")
      pubSubInfoForwarding

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
