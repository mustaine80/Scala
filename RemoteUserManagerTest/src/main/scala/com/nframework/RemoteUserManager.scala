package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props, Timers}
import com.nframework.ControlManager.{TickKey, Update}
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NDouble, NInteger, _}
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._


case class Flight(id: Int, velocity: Double, position: Double)
case class PowerOn(systemID: Int, subsystemID: Int)
case class StartResume(isStart: Int)

object ControlManager {
  private object TickKey
  private object Update

  var DiscoverMap = Map.empty[String, Map[Int, AnyRef]]


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

  //  StartResume
  def serializeStartResume(startResume: StartResume): Array[Byte] = {
    NInteger(startResume.isStart).serialize()._1
  }

  def deserializeStartResume(data: Array[Byte]): StartResume = {
    var offset = 0
    val startResume = NInteger(0)

    startResume.deserialize(data, offset)

    StartResume(startResume.value)
  }
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
    mec ! SendMsg(NMessage("startResume", 0, ControlManager.serializeStartResume(StartResume(20000))))

    Thread.sleep(100)  /// Simulation Manager 가 start event 처리를 하기 위한 시간 확보

    mec ! RegisterMsg("powerOn", 1, managerName)
    mec ! RegisterMsg("powerOn", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage("powerOn", 1, ControlManager.serializePowerOn(PowerOn(1, updateValue + 1))))
      mec ! UpdateMsg(NMessage("powerOn", 2, ControlManager.serializePowerOn(PowerOn(2, updateValue + 10001))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("powerOn", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("powerOn", 2, Array[Byte]()))
    }

    updateValue += 1
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      msg.name match {
        case "flight" =>
          val obj = ControlManager.DiscoverMap.get(msg.name) match {
            case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> Flight(msg.objID, 0, 0))))
            case None => Map(msg.name -> Map(msg.objID -> Flight(msg.objID, 0, 0)))
          }
          ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ obj

        case _ => println("[Control Manager] unregister message. discover fail!")
      }

    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      msg.name match {
        case "flight" =>
          val m = ControlManager.DiscoverMap(msg.name).updated(msg.objID, ControlManager.deserializeFlight(msg.data))
          ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ Map(msg.name -> m)
          println(m)

        case _ => println("[Control Manager] can't find object in discover map. reflect fail!")
      }

    case RecvMsg(msg) =>
      println("[Control Manager] Recv msg received. " + msg)
      msg.name match {
        case "startResume" =>
          val event = ControlManager.deserializeStartResume(msg.data)
          println(event)

        case _ => println("[Control Manager] unknwon interaction message!")
      }

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
