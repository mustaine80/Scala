package com.nframework.meb

import akka.actor.{Actor, ActorRef, Props}
import com.nframework.meb.MEB_Proto.{GetPubsSize, GetSubsSize}
import com.nframework.mec.MEC_Proto._
import com.nframework.mec._


/*  MEB 는 MEC 를 통해 개별 manager NOM 에 대한 pub/sub 정보를 입수한다.
 *
 *  중복 코드에 대한 리팩토링
 *  pubs, subs 에 대한 컬렉션 타입을 immutuable 형태로 처리한다.
 */
object MEB_Proto {
  var mecMap = Map[String, ActorRef]()

  var pubs = Map.empty[String, Set[String]]
  var subs = Map.empty[String, Set[String]]

  def props(test: ActorRef) = {
    Props(new MEB_Proto(test))
  }

  def register(info: PubSubInfo): (Map[String, Set[String]], Map[String, Set[String]]) = info.sharing match {
    case "Publish" =>
      (sharingAdder(pubs, info.msgName, info.managerName),
        sharingRemover(subs, info.msgName, info.managerName))

    case "Subscribe" =>
      (sharingRemover(pubs, info.msgName, info.managerName),
        sharingAdder(subs, info.msgName, info.managerName))

    case "PublishSubscribe" =>
      (sharingAdder(pubs, info.msgName, info.managerName),
        sharingAdder(subs, info.msgName, info.managerName))

    case "Neither" =>
      (sharingRemover(pubs, info.msgName, info.managerName),
        sharingRemover(subs, info.msgName, info.managerName))
  }

  def sharingAdder(items: Map[String, Set[String]], msgName: String, managerName: String): Map[String, Set[String]] = {
    val item = items.get(msgName) match {
      case Some(x) => Map(msgName -> (x ++ Set(managerName)))
      case None => Map(msgName -> Set(managerName))
    }
    items ++ item
  }

  def sharingRemover(items: Map[String, Set[String]], msgName: String, managerName: String): Map[String, Set[String]] = {
    val item = items.get(msgName) match {
      case Some(x) => Map(msgName -> (x -- Set(managerName)))
      case None => Nil
    }
    (items ++ item) filter (_._2.nonEmpty)
  }

  /*  user manager actor 가 resign 또는 MEB detatch 되어 unregister 하는 경우를 가정한다.
   *  이 경우 해당 매니저와 관련된 모든 pub/sub 정보가 제거되어야 한다.
   *
   *  manager name 에 대한 empty Set 처리를 위해 필터를 적용한다.
   */
  def unregister(managerName: String): Unit = {
    pubs.withFilter(_._2.contains(managerName)).map{ _._2 -- Set(managerName) }
    subs.withFilter(_._2.contains(managerName)).map{ _._2 -- Set(managerName) }

    pubs = pubs filter (_._2.nonEmpty)
    subs = subs filter (_._2.nonEmpty)
  }

  /*  MEC 요청을 수신하여 subscriber manager 들을 찾는다. 그리고 mapping 된 msg 를 보낸다.
   *
   */
  def notifyAll(ps: PubMsg, publisher: ActorRef): Unit = ps match {
    case RegisterMsg(msg) =>
      if (pubs.contains(msg.name))
        getSubs(msg.name, publisher).foreach( _ ! DiscoverMsg(msg))
      else
        println("[MEB] RegisterMsg error. Sharing attribute is not 'Publish'. msg : " + msg.name)

    case UpdateMsg(msg) => getSubs(msg.name, publisher).foreach( _ ! ReflectMsg(msg))

    case SendMsg(msg) => getSubs(msg.name, publisher).foreach( _ ! RecvMsg(msg))

    case DeleteMsg(msg) => getSubs(msg.name, publisher).foreach( _ ! RemoveMsg(msg))
  }

  //  todo: cache 형태로 사용할까? detatch 가 자주 일어나는 것은 아니니까. 나중에 처리한다.
  def getSubs(msgName: String, publisher: ActorRef): Seq[ActorRef] =
    subs(msgName).map(MEB_Proto.mecMap(_)).filter(_ != publisher).toSeq


  /*  test 전용이며, testActor 에 MEB actor 상태를 전송하기 위해 사용한다. */
  case class GetPubsSize(test: ActorRef)
  case class GetSubsSize(test: ActorRef)
}


class MEB_Proto(test: ActorRef) extends Actor {  ///  test actor ref 획득을 위한 default parameter
  init()

  def init(): Unit = {
    println("MEB initiaize ...")
  }

  def receive = {
    case MebAttatch(name) => {
      MEB_Proto.mecMap = MEB_Proto.mecMap.updated(name, Sender)
      Sender ! "MEB attatchment success"
    }

    case MebDetatch(name) => {
      MEB_Proto.unregister(name)
      MEB_Proto.mecMap - name
      Sender ! "MEB detatchment success"
    }

    case m: PubSubInfo => {
      val (pubs, subs) = MEB_Proto.register(m)
      MEB_Proto.pubs = pubs
      MEB_Proto.subs = subs
    }

    case PubSubInfoForwarding(userName) => Sender ! "PubSub info forwarding complete"

    //  publisher 에게 자신의 토픽이 되돌아 오는 것을 막기 위해 sender ref 가 필요하다.
    case m: PubMsg => MEB_Proto.notifyAll(m, sender())  /// publisher 를 filter 하기 때문에 Sender 가 아닌 sender()를 사용한다.

    //  다중 JVM 환경에서 actor test 를 위한 코드
    case GetPubsSize(test) => test ! MEB_Proto.pubs
    case GetSubsSize(test) => test ! MEB_Proto.subs

    case m: String => println(s"received $m")
  }

  //  helper for test
  def Sender: ActorRef = {
    if (this.test == null) sender()
    else test
  }
}
