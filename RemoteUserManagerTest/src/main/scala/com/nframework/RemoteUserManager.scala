package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props, Timers}
import com.nframework.Serializer._
import com.nframework.Serializer.NomSerializer._
import com.nframework.ControlManager.{subsMap, TickKey, Update, pubsMap}
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

  var subsMap = Map.empty[(String, Int), NomSerializable]
  var pubsMap = Map.empty[(String, Int), NomSerializable]
}


class ControlManager(meb: ActorRef) extends Actor with Timers {
  val managerName = "Control Manager"
  val mec = context.actorOf(Props(new MEC_Proto(managerName, context.self, meb)), "MEC_ControlManager")

  //  test 용 임시 변수
  var updateValue: Int = 1

  def init(): Unit = {
    println("control manager initialize ...")
    doControl()
  }


  /** test code

    */
  def doControl(): Unit = {
    mec ! SendMsg(NMessage("StartResume", 1, nomInteractionTypeSerializer(StartResume(20000))))

    Thread.sleep(100)  /// Simulation Manager 가 start event 처리를 하기 위한 시간 확보

    val power1 = PowerOn(1,0)
    val power2 = PowerOn(2,0)

    RegisterMessage(power1, 1)
    RegisterMessage(power2, 2)

    println("pubsMap --> " + pubsMap)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }


  //  update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달한다. (32bit flag)
  def update(): Unit = {
    if (updateValue < 1000) {
      val power1 = PowerOn(1, updateValue + 1)
      val power2 = PowerOn(2, updateValue + 10001)

      UpadteMessage(power1, 1, false)
      UpadteMessage(power2, 2)    /// partial serialization
    }

    if (updateValue == 1000) {
      DeleteMessage("PowerOn", 1)
      DeleteMessage("PowerOn", 2)

      println("pubsMap --> " + pubsMap)
    }

    updateValue += 1
  }

  def receive = {
    //  mec -> user

    //  Discover를 위해 객체 생성을 담당하는 객체(User Manager)에서 NOM schema 를 이용하여 인스턴스를 일관성 있게 생성해야 한다.
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      subsMap = subsMap.updated((msg.name, msg.objID), deserializer(getDefaultNom(msg.name), msg.data))
      println("subsMap: " + subsMap)

    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      subsMap = subsMap.updated((msg.name, msg.objID), deserializer(subsMap(msg.name, msg.objID), msg.data))
      println("subsMap: " + subsMap)

    case RecvMsg(msg) =>
      println("[Control Manager] Recv msg received. " + msg)
      val event = deserializer(getDefaultNom(msg.name), msg.data)
      println(event)

    case RemoveMsg(msg) =>
      println("[Control Manager] Remove msg received. " + msg)
      subsMap = subsMap - ((msg.name, msg.objID))
      println("subsMap: " + subsMap)

    case Update => update()

    case PubSubInfoForwarding(userName) => init()

    case _ => println("[Control Manager] unknown Pub/Sub message. actor receive function fail!")
  }


  //  Wrapper
  def RegisterMessage(s: NomSerializable, id: Int): Unit = {
    mec ! RegisterMsg(NMessage(s.getName(), id, nomObjectTypeSerializer(s, 0xFFFFFFFF)))
    pubsMap = pubsMap.updated((s.getName(), id), s)
  }

  //  default parameter 는 부분 직렬화를 지원한다.
  def UpadteMessage(s: NomSerializable, objId: Int, partialSerialization: Boolean = true): Unit = {
    if (partialSerialization) {
      val updateFlag = compareObject(pubsMap(s.getName(), objId), s)
      mec ! UpdateMsg(NMessage(s.getName(), objId, nomObjectTypeSerializer(s, updateFlag)))
      pubsMap = pubsMap.updated((s.getName(), objId), s)
    }
    else
      mec ! UpdateMsg(NMessage(s.getName(), objId, nomObjectTypeSerializer(s, 0xFFFFFFFF)))
  }

  def DeleteMessage(msgName: String, objId: Int): Unit = {
    mec ! DeleteMsg(NMessage(msgName, objId, Array[Byte]()))
    pubsMap = pubsMap - ((msgName, objId))
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
