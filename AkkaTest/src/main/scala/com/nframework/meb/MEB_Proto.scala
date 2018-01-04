package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.{MebAttatch, MebDetatch, PubSubInfo, PubSubInfoForwarding}
import com.nframework.mec._


/*  MEB 는 MEC 를 통해 개별 manager NOM 에 대한 pub/sub 정보를 입수한다.
 *
 *  중복 코드에 대한 리팩토링
 *  pubs, subs 에 대한 컬렉션 타입을 immutuable 형태로 처리한다.
 */
object PubSubTable {
  var pubs = Map.empty[String, Set[String]]
  var subs = Map.empty[String, Set[String]]

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
      case None => Map(msgName -> Set(managerName))
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
  def notifyAll(ps: PubSub, publisher: ActorRef): Unit = ps match {
    case RegisterMsg(msg) =>
      if (pubs.contains(msg.name)) {
        subs(msg.name).foreach { x =>
          val subscriber = MEB_Proto.mecMap(x)
          if (subscriber != publisher)
            subscriber ! DiscoverMsg(msg)
        }
      } else
        println("[MEB] RegisterMsg error. Sharing attribute is not 'Publish'. msg : " + msg.name)

    case UpdateMsg(msg) =>
      subs(msg.name).foreach{ x =>
        val subscriber = MEB_Proto.mecMap(x)
        if (subscriber != publisher)
          subscriber ! ReflectMsg(msg) }


    case SendMsg(msg) =>
      subs(msg.name).foreach{ x =>
        val subscriber = MEB_Proto.mecMap(x)
        if (subscriber != publisher)
          subscriber ! RecvMsg(msg) }

    case DeleteMsg(msg) =>
      subs(msg.name).foreach{ x =>
        val subscriber = MEB_Proto.mecMap(x)
        if (subscriber != publisher)
          subscriber ! RemoveMsg(msg) }
  }
}


object MEB_Proto {
  var mecMap = Map[String, ActorRef]()
}


class MEB_Proto extends Actor {
  init()

  def init(): Unit = {
    println("MEB initiaize ...")
  }

  def receive = {
    case MebAttatch(name) => {
      MEB_Proto.mecMap = MEB_Proto.mecMap.updated(name, sender())
      sender() ! "MEB attatchment success"
    }

    case MebDetatch(name) => {
      PubSubTable.unregister(name)
      MEB_Proto.mecMap - name
      sender() ! "MEB detatchment success"
    }

    case m: PubSubInfo => {
      println("MEB Pub/Sub register, " + m)

      val (pubs, subs) = PubSubTable.register(m)
      PubSubTable.pubs = pubs
      PubSubTable.subs = subs

      println("MEB pusbs. " + PubSubTable.pubs)
      println("MEB susbs. " + PubSubTable.subs)
    }

    case PubSubInfoForwarding(userName) => sender() ! "PubSub info forwarding complete"

    //  publisher 에게 자신의 토픽이 되돌아 오는 것을 막기 위해 sender ref 가 필요하다.
    case m: PubSub => PubSubTable.notifyAll(m, sender())

    case m: String => println(s"received $m")
  }
}
