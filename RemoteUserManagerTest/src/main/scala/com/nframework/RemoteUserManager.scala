package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props}
import com.nframework.mec._
import com.nframework.nom.{NDouble, NInteger, _}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable


case class Flight(id: Int, velocity: Double, position: Double)
case class PowerOn(systemID: Int, subsystemID: Int)

object ControlManager {
  /** todo: NOMParser 의 parsing 정보는 1회만 생성하고 이를 가져다 쓰는 방식이어야 하는데, 현재는 필요시 생성하여 쓰는 방식이다.
    * immutable 하게 생성하여 get 하는 방식으로 보완해야 한다.
    */
  Proto_NOMParser.parse("src/main/Resources/test.json")
  val objects = Proto_NOMParser.objectTypes
  val interactions = Proto_NOMParser.interactionTypes

  var DiscoverMap = mutable.Map.empty[String, Function1[Array[Byte], AnyRef]]


  /** 발행(pub)할 토픽 정보를 직렬화 하는 함수를 사용자가 구현한다.
    * 구독(sub)할 토픽 정보를 역직렬화 하는 함수를 사용자가 구현한다.
    * todo: NOM schema 를 이용하여 자동화할 필요가 있다.
    */

  //  Flight
  def serializeFlight(flight: Flight): Array[Byte] = {
    NInteger(flight.id).serialize()._1 ++
      NDouble(flight.velocity).serialize()._1 ++
      NDouble(flight.position).serialize()._1
  }

  def deserializeFlight(data: Array[Byte]): Flight = {
    var offset = 0
    val id = NInteger(0)
    val velocity = NDouble(0.0)
    val position = NDouble(0.0)

    offset = id.deserialize(data, offset)
    offset += velocity.deserialize(data, offset)
    offset += position.deserialize(data, offset)

    Flight(id.value, velocity.value, position.value)
  }


  //  PowerOn
  def serializePowerOn(powerOn: PowerOn): Array[Byte] = {
    NInteger(powerOn.systemID).serialize()._1 ++
      NInteger(powerOn.subsystemID).serialize()._1
  }

  def deserializePowerOn(data: Array[Byte]): PowerOn = {
    var offset = 0
    val systemID = NInteger(0)
    val subsystemID = NInteger(0)


    offset = systemID.deserialize(data, offset)
    offset = subsystemID.deserialize(data, offset)

    PowerOn(systemID.value, subsystemID.value)
  }

}


class ControlManager(meb: ActorRef) extends Actor {
  val managerName = "Control Manager"
  val mec = context.actorOf(Props(new MEC_Proto(managerName, context.self, meb)), "MEC_ControlManager")

  init()

  def init(): Unit = {
    println("control manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요
    doControl()
  }



  /** test code

    */
  def doControl(): Unit = {

    mec ! RegisterMsg("powerOn", managerName)

    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(1, 101))))
    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(2, 201))))
    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(3, 301))))
    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(4, 401))))
    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(5, 501))))
    mec ! UpdateMsg(NMessage("powerOn", ControlManager.serializePowerOn(PowerOn(6, 601))))
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    //  todo: need to NOM serialization
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      msg.name match {
        case "flight" =>
          ControlManager.DiscoverMap += (msg.name -> ControlManager.deserializeFlight)

        case "powerOn" =>
          ControlManager.DiscoverMap += (msg.name -> ControlManager.deserializePowerOn)

        case _ => println("[Control Manager] msg is not register. fail discover!")
      }

    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      ControlManager.DiscoverMap.get(msg.name) match {
        case Some(deserializer) =>
          val obj = deserializer(msg.data)
          println("[Control Manager] " + obj.toString)

        case None => println("[Control Manager] msg is not discover. fail reflect!")
      }

    case RecvMsg(msg) => println("[Control Manager] Recv msg received. " + msg)

    case RemoveMsg(msg) => println("[Control Manager] Remove msg received. " + msg)
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
