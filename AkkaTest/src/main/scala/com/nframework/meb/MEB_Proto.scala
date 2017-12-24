package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.{MebAttatch, PubSubInfo, PubSubInfoForwarding}
import com.nframework.mec._
import com.nframework.nom.NMessage

import scala.collection.mutable


/*  MEB 는 MEC 를 통해 개별 manager NOM 에 대한 pub/sub 정보를 입수한다.
 *
 *  todo: 중복 코드에 대한 리팩토링 필요
 *  pubs, subs 에 대한 컬렉션 타입을 immutuable 형태로 처리한다.
 */
object PubSubTable {
  var pubs = Map[String, Set[String]]()
  var subs = Map[String, Set[String]]()

  def register(info: PubSubInfo): Unit = info.sharing match {
    case "Publish" =>
      val pub = pubs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x ++ Set(info.managerName)))
        case None => Map(info.msgName -> Set(info.managerName))
      }
      pubs = pubs ++ pub

      val sub = subs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x -- Set(info.managerName)))
        case None => Nil
      }
      subs = subs ++ sub
      subs = subs filter (_._2.nonEmpty)

    case "Subscribe" =>
      val pub = pubs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x -- Set(info.managerName)))
        case None => Nil
      }
      pubs = pubs ++ pub
      pubs = pubs filter (_._2.nonEmpty)

      val sub = subs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x ++ Set(info.managerName)))
        case None => Map(info.msgName -> Set(info.managerName))
      }
      subs = subs ++ sub

    case "PublishSubscribe" =>
      val pub = pubs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x ++ Set(info.managerName)))
        case None => Map(info.msgName -> Set(info.managerName))
      }
      pubs = pubs ++ pub

      val sub = subs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x ++ Set(info.managerName)))
        case None => Map(info.msgName -> Set(info.managerName))
      }
      subs = subs ++ sub

    case "Neither" =>
      val pub = pubs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x -- Set(info.managerName)))
        case None => Nil
      }
      pubs = pubs ++ pub
      pubs = pubs filter (_._2.nonEmpty)

      val sub = subs.get(info.msgName) match {
        case Some(x) => Map(info.msgName -> (x -- Set(info.managerName)))
        case None => Nil
      }
      subs = subs ++ sub
      subs = subs filter (_._2.nonEmpty)
  }

  /*  user manager actor 가 resign 되어 unregister 하는 경우를 가정한다.
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
   *  현재 BooPickle 을 사용한 구현에서는 객체 Replication 을 위한 buffer 가 필요없다.
   *
   *  ReflectMsg 는 buffer 없는 구현을 사용한다.
   *
   *  todo: update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달할 수 있는 기능이 필요하다.
   */
  def notifyAll(ps: PubSub, publisher: ActorRef): Unit = ps match {
    case RegisterMsg(msgName, objID, userName) =>
      if (pubs.contains(msgName))
        subs(msgName).filter(_ != userName).foreach(MEB_Proto.mecMap(_)
          ! DiscoverMsg(NMessage(msgName, objID, Array[Byte](0))))    /// Discover 정보는 msgName 만 사용한다.
      else
        println("[MEB] RegisterMsg error. Sharing attribute is not 'Publish'. msg : " + msgName)

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
  val mecMap = mutable.Map[String, ActorRef]()
}


class MEB_Proto extends Actor {
  init()

  def init(): Unit = {
    println("MEB initiaize ...")
  }


  //  todo: need to implement
  def receive = {
    case MebAttatch(name) => {
      MEB_Proto.mecMap(name) = sender()
      sender() ! "MEB attatchment success"
    }

    case m: PubSubInfo => {
      println("MEB Pub/Sub register, " + m)
      PubSubTable.register(m)
      println("MEB pusbs. " + PubSubTable.pubs)
      println("MEB susbs. " + PubSubTable.subs)
    }

    case PubSubInfoForwarding(userName) => sender() ! "PubSub info forwarding complete"

    //  publisher 에게 자신의 토픽이 되돌아 오는 것을 막기 위해 sender ref 가 필요하다.
    case m: PubSub => PubSubTable.notifyAll(m, sender())

    case m: String => println(s"received $m")
  }
}
