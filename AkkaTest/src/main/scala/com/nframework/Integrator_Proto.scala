package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.nframework.meb.MEB_Proto
import com.nframework.mec._
import com.nframework.nom._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable


case class Flight(id: Int, velocity: Double, position: Double)
case class PowerOn(systemID: Int, subsystemID: Int)


object SimulationManager {
  /** todo: NOMParser 의 parsing 정보는 1회만 생성하고 이를 가져다 쓰는 방식이어야 하는데, 현재는 필요시 생성하여 쓰는 방식이다.
    * immutable 하게 생성하여 get 하는 방식으로 보완해야 한다.
   */
  Proto_NOMParser.parse("src/main/Resources/test.json")
  val objects = Proto_NOMParser.objectTypes
  val interactions = Proto_NOMParser.interactionTypes

  var DiscoverMap = mutable.Map.empty[String, Function1[Array[Byte], AnyRef]]


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
}


class SimulationManager(meb: ActorRef) extends Actor {

  val managerName = "Simulation Manager"
  val mec = context.actorOf(Props(new MEC_Proto("Simulation Manager", context.self, meb)), "MEC_SimulationManager")

  init()

  def init(): Unit = {
    println("simulation manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요
    doFlight()
  }


  /** test code

   */
  def doFlight(): Unit = {

    mec ! RegisterMsg("flight", managerName)

    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(1, 100.5, 10.0))))
    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(2, 300.0, 100.0))))
    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(1, 130.5, 20.0))))
    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(2, 350.0, 110.0))))
    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(1, 160.5, 30.0))))
    mec ! UpdateMsg(NMessage("flight", SimulationManager.serializeFlight(Flight(2, 400.0, 120.0))))
  }


  //  todo: need to implement
  def receive = {
    //  mec -> user
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      msg.name match {
        case "flight" =>
          SimulationManager.DiscoverMap += (msg.name -> SimulationManager.deserializeFlight)

        case "powerOn" =>
          SimulationManager.DiscoverMap += (msg.name -> SimulationManager.deserializePowerOn)

        case _ => println("[Simulation Manager] msg is not register. fail discover!")
      }

    case ReflectMsg(msg) =>
      println("[Simulation Manager] Reflect msg received. " + msg)
      SimulationManager.DiscoverMap.get(msg.name) match {
        case Some(deserializer) =>
          val obj = deserializer(msg.data)
          println("[Simulation Manager] " + obj.toString)

        case None => println("[Simulation Manager] msg is not discover. fail reflect!")
      }

    case RecvMsg(msg) => println("[Simulation Manager] Recv msg received. " + msg)

    case RemoveMsg(msg) => println("[Simulation Manager] Remove msg received. " + msg)
  }
}


object Integrator_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("server")
    val system = ActorSystem("server", config)

    //  todo: need to create integrator level actor?
    val meb = system.actorOf(Props[MEB_Proto], "MEB")
    val simulationManager = system.actorOf(Props(new SimulationManager(meb)), "SimulationManager")
  }
}
