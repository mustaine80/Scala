package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.SimulationManager.{TickKey, Update}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom._
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._


case class Flight(id: Int, velocity: Double, position: Double)
case class PowerOn(systemID: Int, subsystemID: Int)
case class StartResume(isStart: Int)


object SimulationManager {
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
    offset += subsystemID.deserialize(data, offset)

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


class SimulationManager(meb: ActorRef) extends Actor with Timers {
  val managerName = "Simulation Manager"
  val mec = context.actorOf(Props(new MEC_Proto("Simulation Manager", context.self, meb)), "MEC_SimulationManager")

  //  test 용 임시 변수
  var updateValue: Int = 0

  def init(): Unit = {
    println("simulation manager initialize ...")
  }


  /** test code

   */
  def doFlight(): Unit = {

    mec ! RegisterMsg("flight", 1, managerName)
    mec ! RegisterMsg("flight", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage("flight", 1, SimulationManager.serializeFlight(Flight(1, updateValue * 10.0, updateValue * 50.0))))
      mec ! UpdateMsg(NMessage("flight", 2, SimulationManager.serializeFlight(Flight(2, updateValue * 20.0, updateValue * 100.0))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("flight", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("flight", 2, Array[Byte]()))
    }

    updateValue += 1
  }

  def procStartResume(event: StartResume): Unit = doFlight()

  //  todo: need to implement
  def receive = {
    //  mec -> user
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      msg.name match {
        case "powerOn" =>
          val obj = SimulationManager.DiscoverMap.get(msg.name) match {
            case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> PowerOn(0, 0))))
            case None => Map(msg.name -> Map(msg.objID -> PowerOn(0, 0)))
          }
          SimulationManager.DiscoverMap = SimulationManager.DiscoverMap ++ obj

        case _ => println("[Simulation Manager] unregister message. discover fail!")
      }

    case ReflectMsg(msg) =>
      println("[Simulation Manager] Reflect msg received. " + msg)
      msg.name match {
        case "powerOn" =>
          val m = SimulationManager.DiscoverMap(msg.name).updated(msg.objID, SimulationManager.deserializePowerOn(msg.data))
          SimulationManager.DiscoverMap = SimulationManager.DiscoverMap ++ Map(msg.name -> m)
          println(m)

        case _ => println("[Simulation Manager] can't find object in discover map. reflect fail!")
      }

    case RecvMsg(msg) =>
      println("[Simulation Manager] Recv msg received. " + msg)
      msg.name match {
        case "startResume" =>
          val event = SimulationManager.deserializeStartResume(msg.data)
          procStartResume(event)

        case _ => println("[Simulation Manager] unknwon interaction message!")
      }

    case RemoveMsg(msg) =>
      println("[Simulation Manager] Remove msg received. " + msg)
      val m = SimulationManager.DiscoverMap(msg.name) - msg.objID
      SimulationManager.DiscoverMap = SimulationManager.DiscoverMap.updated(msg.name, m)
      println(SimulationManager.DiscoverMap)

    case SimulationManager.Update => update()

    case PubSubInfoForwarding(userName) => init()

    case _ => println("[Simulation Manager] unknown Pub/Sub message. actor receive function fail!")
  }
}


object Integrator_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("server")
    val system = ActorSystem("server", config)

    val meb = system.actorOf(Props[MEB_Proto], "MEB")
    val simulationManager = system.actorOf(Props(new SimulationManager(meb)), "SimulationManager")
  }
}
