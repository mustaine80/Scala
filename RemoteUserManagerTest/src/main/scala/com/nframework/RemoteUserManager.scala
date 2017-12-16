package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props}
import com.typesafe.config.ConfigFactory
import com.nframework.mec._
import com.nframework.nom._

object ControlManager {
  /** todo: NOMParser 의 parsing 정보는 1회만 생성하고 이를 가져다 쓰는 방식이어야 하는데, 현재는 필요시 생성하여 쓰는 방식이다.
    * immutable 하게 생성하여 get 하는 방식으로 보완해야 한다.
    */
  NOMParser.parse("src/main/Resources/test.json")
  val objects = Proto_NOMParser.objectTypes
  val interactions = Proto_NOMParser.interactionTypes
}


class ControlManager(meb: ActorRef) extends Actor {
  val mec = context.actorOf(Props(new MEC_Proto("ControlManager", context.self, meb)), "MEC_ControlManager")
  val managerName = "Control Manager"

  init()

  def init(): Unit = {
    println("control manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요
    doControl()
  }


  /** todo: NOM Parser 에 있는 객체 모델을 이용하여 NOM 템플릿을 제공한다.
    * Manager 는 템플릿 객체를 받아와서 자신의 Pub 정보를 기술해야 한다.
    */
  def getNOMTemplate(template: String): List[NOM] = {
    ControlManager.objects.get(template) match {
      case Some(x) => DummyHead(template) :: x.fields.map{
        case Field_Proto(name, model, size, fixedLength, indicator) =>
          (DummyNOM(name, NChar_Dummy('C'))) }.toList   /// todo: 일단 임시로 dummy 하게 만들어 본다. 아래처럼 구현해야 한다.
      //  NType(name) :: NType(model) * size in loop :: fixedLength(TBD) :: indicator(TBD)
      case None => println("[Manager] requested object NOM template is not available."); Nil
    }
  }


  /** test code
    * List[0] 를 header 처럼 사용한다.
    */
  def doControl(): Unit = {
    val control1 = getNOMTemplate("object1")
    mec ! RegisterMsg(control1(0).getName, managerName)
    mec ! UpdateMsg(control1)

    val control2 = getNOMTemplate("object2")
    mec ! RegisterMsg(control2(0).getName, managerName)
    mec ! UpdateMsg(control2)
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    //  todo: need to NOM serialization
    case DiscoverMsg(msg) => println("[Control Manager] discover msg received. " + msg)
    case ReflectMsg(msg) =>
    case RecvMsg(msg) =>
    case RemoveMsg(msg) =>
  }
}


class RemoteUserManager_Test(selection: ActorSelection, path: String) extends Actor {
  selection ! Identify(None)

  //  todo: MEB actor that do not work should be considered
  def receive = {
    case ActorIdentity(_, Some(actorRef)) =>
      val controlManager = context.actorOf(Props(new ControlManager(actorRef)), "ControlManager")

    case _ => println("not implement for remote actor")
  }
}

object RemoteUserManager_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("client")
    val system = ActorSystem("client", config)

    val path = "akka.tcp://server@0.0.0.0:2551/user/MEB"
    val selection = system.actorSelection(path)

    val remote = system.actorOf(Props(new RemoteUserManager_Test(selection, path)), "remote")
  }
}
