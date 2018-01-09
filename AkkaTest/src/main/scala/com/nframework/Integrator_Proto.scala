package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.Serializer.NomSerializer._
import com.nframework.Serializer._
import com.nframework.SimulationManager.{DiscoverMap, TickKey, Update, UpdateMap}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom._
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._


case class Position(x: Double, y: Double, z: Double) extends NomSerializable

case class Flight(id: Int, velocity: Double, position: Position) extends NomSerializable

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable

case class StartResume(isStart: Int) extends NomSerializable


object SimulationManager {
  private object TickKey
  private object Update

  //  multi-Map 을 사용하는 것보다 Tuple key 를 사용하는 것이 가독성에 더 유리할 것으로 판단되어 변경한다.
  var DiscoverMap = Map.empty[(String, Int), NomSerializable]
  var UpdateMap = Map.empty[(String, Int), NomSerializable]
}


class SimulationManager(meb: ActorRef) extends Actor with Timers {
  val managerName = "Simulation Manager"
  val mec = context.actorOf(Props(new MEC_Proto("Simulation Manager", context.self, meb)), "MEC_SimulationManager")

  //  test 용 임시 변수
  var updateValue: Int = 1

  def init(): Unit = {
    println("simulation manager initialize ...")
  }

  /** test code

   */
  def doFlight(): Unit = {
    val flight1 = Flight(1, 0.0, Position(0.0, 0.0, 0.0))
    val flight2 = Flight(2, 0.0, Position(0.0, 0.0, 0.0))

    RegisterMessage(flight1, 1)
    RegisterMessage(flight2, 2)

    println("UpdateMap --> " + UpdateMap)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }


  //  update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달한다. (32bit flag)
  def update(): Unit = {
    if (updateValue < 1000) {
      val flight1 = Flight(1, updateValue * 10.0, Position(updateValue * 50.0, updateValue * 30.0, updateValue * 10.0))
      val flight2 = Flight(2, updateValue * 20.0, Position(updateValue * 100.0, updateValue * 50.0, updateValue * 20.0))

      UpadteMessage(flight1, 1, false)
      UpadteMessage(flight2, 2)  /// partial serialization
    }

    if (updateValue == 1000) {
      DeleteMessage("Flight", 1)
      DeleteMessage("Flight", 2)

      println("UpdateMap --> " + UpdateMap)
    }

    updateValue += 1
  }


  def receive = {
    //  mec -> user
    //  Discover를 위해 객체 생성을 담당하는 객체(User Manager)에서 NOM schema 를 이용하여 인스턴스를 일관성 있게 생성해야 한다.
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      DiscoverMap = DiscoverMap.updated((msg.name, msg.objID), nomDeserializer(getDefaultNOMSerializable(msg.name), msg.data))
      println("DiscoverMap: " + DiscoverMap)

    case ReflectMsg(msg) =>
      println("[Simulation Manager] Reflect msg received. " + msg)
      DiscoverMap = DiscoverMap.updated((msg.name, msg.objID), nomDeserializer(DiscoverMap(msg.name, msg.objID), msg.data))
      println("DiscoverMap: " + DiscoverMap)

    case RecvMsg(msg) =>
      println("[Simulation Manager] Recv msg received. " + msg)
      val event = nomDeserializer(getDefaultNOMSerializable(msg.name), msg.data)
      println(event)
      //  test code
      if (msg.name == "StartResume") doFlight()

    case RemoveMsg(msg) =>
      println("[Simulation Manager] Remove msg received. " + msg)
      DiscoverMap = DiscoverMap - ((msg.name, msg.objID))
      println("DiscoverMap: " + DiscoverMap)

    case Update => update()

    case PubSubInfoForwarding(userName) => init()

    case _ => println("[Simulation Manager] unknown Pub/Sub message. actor receive function fail!")
  }


  //  Wrapper
  def RegisterMessage(s: NomSerializable, id: Int): Unit = {
    mec ! RegisterMsg(NMessage(s.getName(), id, nomObjectTypeSerializer(s, 0xFFFFFFFF)))
    UpdateMap = UpdateMap.updated((s.getName(), id), s)
  }

  //  default parameter 는 부분 직렬화를 지원한다.
  def UpadteMessage(s: NomSerializable, objId: Int, partialSerialization: Boolean = true): Unit = {
    if (partialSerialization) {
      val updateFlag = compareObject(UpdateMap(s.getName(), objId), s)
      mec ! UpdateMsg(NMessage(s.getName(), objId, nomObjectTypeSerializer(s, updateFlag)))
      UpdateMap = UpdateMap.updated((s.getName(), objId), s)
    }
    else
      mec ! UpdateMsg(NMessage(s.getName(), objId, nomObjectTypeSerializer(s, 0xFFFFFFFF)))
  }

  def DeleteMessage(msgName: String, objId: Int): Unit = {
    mec ! DeleteMsg(NMessage(msgName, objId, Array[Byte]()))
    UpdateMap = UpdateMap - ((msgName, objId))
  }
}


object Integrator_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("server")
    val system = ActorSystem("server", config)

    val meb = system.actorOf(MEB_Proto.props(null), "MEB")
    val simulationManager = system.actorOf(Props(new SimulationManager(meb)), "SimulationManager")
  }
}
