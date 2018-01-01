package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props, Timers}
import com.nframework.ControlManager.{TickKey, Update}
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom._
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._


case class Position(x: Double, y: Double, z: Double) extends NomSerializable

case class Flight(id: Int, velocity: Double, position: Position) extends NomSerializable

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable

case class StartResume(isStart: Int) extends NomSerializable


object ControlManager {
  private object TickKey
  private object Update

  var DiscoverMap = Map.empty[String, Map[Int, NomSerializable]]
}


class ControlManager(meb: ActorRef) extends Actor with Timers {
  val managerName = "Control Manager"
  val mec = context.actorOf(Props(new MEC_Proto(managerName, context.self, meb)), "MEC_ControlManager")

  //  test 용 임시 변수
  var updateValue: Int = 0

  def init(): Unit = {
    println("control manager initialize ...")
    doControl()
  }


  /** test code

    */
  def doControl(): Unit = {
    mec ! SendMsg(NMessage("StartResume", 0, Proto_NOMParser.nomInteractionTypeSerializer(StartResume(20000))))

    Thread.sleep(100)  /// Simulation Manager 가 start event 처리를 하기 위한 시간 확보

    mec ! RegisterMsg("PowerOn", 1, managerName)
    mec ! RegisterMsg("PowerOn", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage("PowerOn", 1, Proto_NOMParser.nomObjectTypeSerializer(PowerOn(1, updateValue + 1))))
      mec ! UpdateMsg(NMessage("PowerOn", 2, Proto_NOMParser.nomObjectTypeSerializer(PowerOn(2, updateValue + 10001))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("PowerOn", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("PowerOn", 2, Array[Byte]()))
    }

    updateValue += 1
  }


  def receive = {
    //  mec -> user
    //  obj id 별 객체를 생성할 필요가 굳이 없을것 같긴한데... 추후에 불변객체 생성 방식이 아닌 객체 변경 형태로 변경될 것을
    //  감안하여 현 상태를 유지한다.
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      val obj = ControlManager.DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> Proto_NOMParser.getDefaultNOMSerializable(msg.name))))
        case None => Map(msg.name -> Map(msg.objID -> Proto_NOMParser.getDefaultNOMSerializable(msg.name)))
      }

      ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ obj

    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      val obj = ControlManager.DiscoverMap(msg.name)
        .updated(msg.objID, Proto_NOMParser
          .nomObjectTypeDeserializer(ControlManager.DiscoverMap(msg.name)(msg.objID), msg.data))

      ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ Map(msg.name -> obj)
      println(obj)

    case RecvMsg(msg) =>
      println("[Control Manager] Recv msg received. " + msg)
      val event = Proto_NOMParser.nomInteractionTypeDeserializer(Proto_NOMParser.getDefaultNOMSerializable(msg.name), msg.data)
      println(event)

    case RemoveMsg(msg) =>
      println("[Control Manager] Remove msg received. " + msg)
      val m = ControlManager.DiscoverMap(msg.name) - msg.objID
      ControlManager.DiscoverMap = ControlManager.DiscoverMap.updated(msg.name, m)
      println(ControlManager.DiscoverMap)

    case Update => update()

    case PubSubInfoForwarding(userName) => init()

    case _ => println("[Control Manager] unknown Pub/Sub message. actor receive function fail!")
  }
}


class RemoteUserManager_Test(selection: ActorSelection, path: String) extends Actor {
  selection ! Identify(None)

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
