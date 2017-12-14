package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.{MebAttatch, PubSubInfo}
import com.nframework.mec._
import com.nframework.nom.{DummyNOM, NChar_Dummy}

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
   *  todo: Update 를 처리하기 위해서는 MEC 로부터 실제 메시지 객체에 대한 직렬화된 바이트 스트림이 넘어와야 한다.
   *  UpdateMsg(nom, serializedObject), ReflectMSg(nom, deserializedObject) 이렇게 구성해야 할 것 같은테 직렬화 처리가 아직 안되어 있음
   *  pending 한다.
   */
  def lookup(msg: PubSub): Unit = msg match {
    case RegisterMsg(msgName, userName) =>
      println(subs.keys)
      subs(msgName).foreach(MEB_Proto.mecMap(_) ! DiscoverMsg(DummyNOM(msgName, NChar_Dummy('a'))))

    case UpdateMsg(msg) =>
      subs(msg.getName).foreach(MEB_Proto.mecMap(_) ! ReflectMsg(DummyNOM(msg.getName, NChar_Dummy('a')), Array[Byte](5)))

    case SendMsg(msg) =>
      subs(msg.getName).foreach(MEB_Proto.mecMap(_) ! RecvMsg(DummyNOM(msg.getName, NChar_Dummy('a'))))

    case DeleteMsg(msg) =>
      subs(msg.getName).foreach(MEB_Proto.mecMap(_) ! RemoveMsg(DummyNOM(msg.getName, NChar_Dummy('a'))))
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
    }

    case m: PubSub => PubSubTable.lookup(m)

    case m: String => println(s"received $m")
  }
}
