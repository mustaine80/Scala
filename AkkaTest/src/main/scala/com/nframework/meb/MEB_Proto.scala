package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.{MebAttatch, PubSubInfo}
import com.nframework.mec._
import com.nframework.nom.{DummyNOM, NChar_Dummy, NOM}

import scala.collection.mutable


/*  MEB 는 MEC 를 통해 개별 manager NOM 에 대한 pub/sub 정보를 입수한다.
 *  todo: 중복 코드에 대한 리팩토링 필요
 *  todo: pubs, subs 에 대한 컬렉션 타입을 val/immutuable 형태로 변경해야 한다.
 */
object PubSubTable {
  var pubs = mutable.Map[String, mutable.Set[String]]()
  var subs = mutable.Map[String, mutable.Set[String]]()

  def register(info: PubSubInfo): Unit = info.sharing match {
    case "Publish" =>
      pubs.get(info.msgName) match {
        case Some(x) => x += info.managerName
        case None => pubs(info.msgName) = mutable.Set[String](info.managerName)
      }

      subs.get(info.msgName) match {
        case Some(x) => x -= info.managerName
        case None =>
      }

    case "Subscribe" =>
      pubs.get(info.msgName) match {
        case Some(x) => x -= info.managerName
        case None =>
      }

      subs.get(info.msgName) match {
        case Some(x) => x += info.managerName
        case None => subs(info.msgName) = mutable.Set[String](info.managerName)
      }

    case "PublishSubscribe" =>
      pubs.get(info.msgName) match {
        case Some(x) => x += info.managerName
        case None => pubs(info.msgName) = mutable.Set[String](info.managerName)
      }

      subs.get(info.msgName) match {
        case Some(x) => x += info.managerName
        case None => subs(info.msgName) = mutable.Set[String](info.managerName)
      }

    case "Neither" =>
      pubs.get(info.msgName) match {
        case Some(x) => x -= info.managerName
        case None =>
      }

      subs.get(info.msgName) match {
        case Some(x) => x -= info.managerName
        case None =>
      }
  }

  /*  user manager actor 가 resign 되어 unregister 하는 경우를 가정한다.
   *  이 경우 해당 매니저와 관련된 모든 pub/sub 정보가 제거되어야 한다.
   *
   *  todo: empty Set 문제가 있을 것 같은데... 추후에 처리한다.
   */
  def unregister(managerName: String): Unit = {
    pubs.withFilter(_._2.contains(managerName)).map{ case (x, y) => y -= managerName }
    subs.withFilter(_._2.contains(managerName)).map{ case (x, y) => y -= managerName }
  }

  /*  MEC 요청을 수신하여 subscriber manager 들을 찾는다. 그리고 mapping 된 msg 를 보낸다.
   *  현재 BooPickle 을 사용한 구현에서는 객체 Replication 을 위한 buffer 가 필요없다.
   *
   *  todo: update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달할 수 있는 기능이 필요할 것 같다.
   *
   *  TODO: 일단 ReflectMsg 는 buffer 없는 구현을 사용한다. 추후 NOM 구현과 PubSubSerializer 변경에 따라 달라져야 한다.   *
   */
  def notifyAll(ps: PubSub): Unit = ps match {
    case RegisterMsg(msgName, userName) =>
      if (pubs.contains(msgName))
        subs(msgName).foreach(MEB_Proto.mecMap(_)
          ! DiscoverMsg(List[NOM](DummyNOM(msgName, NChar_Dummy('a')))))    /// todo: dummy Impl
      else
        println("RegisterMsg error. Sharing attribute is not 'Publish'")

    case UpdateMsg(msg) =>
      subs(msg(0).getName).foreach(MEB_Proto.mecMap(_)
        ! ReflectMsg(List[NOM](DummyNOM(msg(0).getName, NChar_Dummy('a')))))  /// todo: dummy Impl

    case SendMsg(msg) =>
      subs(msg(0).getName).foreach(MEB_Proto.mecMap(_)
        ! RecvMsg(List[NOM](DummyNOM(msg(0).getName, NChar_Dummy('a'))))) /// todo: dummy Impl

    case DeleteMsg(msg) =>
      subs(msg(0).getName).foreach(MEB_Proto.mecMap(_)
        ! RemoveMsg(List[NOM](DummyNOM(msg(0).getName, NChar_Dummy('a'))))) /// todo: dummy Impl
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

    case m: PubSub =>
      println("MEB pub/sub msg received...")
      PubSubTable.notifyAll(m)

    case m: String => println(s"received $m")
  }
}
