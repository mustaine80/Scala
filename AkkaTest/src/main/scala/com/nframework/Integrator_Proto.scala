package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.SimulationManager.{TickKey, Update}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NomSerializable, _}
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._


case class Position(x: Double, y: Double, z: Double) extends NomSerializable

case class Flight(id: Int, velocity: Double, position: Position) extends NomSerializable

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable

case class StartResume(isStart: Int) extends NomSerializable


object SimulationManager {
  private object TickKey
  private object Update

  var DiscoverMap = Map.empty[String, Map[Int, NomSerializable]]
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

    mec ! RegisterMsg("Flight", 1, managerName)
    mec ! RegisterMsg("Flight", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  /**
    * todo: update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달할 수 있는 기능이 필요하다.
    *       32bit flag ??
    */
  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage(
        "Flight", 1, Proto_NOMParser.nomObjectTypeSerializer(Flight(1, updateValue * 10.0,
          Position(updateValue * 50.0, updateValue * 30.0, updateValue * 10.0)))))

      mec ! UpdateMsg(NMessage(
        "Flight", 2, Proto_NOMParser.nomObjectTypeSerializer(Flight(2, updateValue * 20.0,
          Position(updateValue * 100.0, updateValue * 50.0, updateValue * 20.0)))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("Flight", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("Flight", 2, Array[Byte]()))
    }

    updateValue += 1
  }


  def receive = {
    //  mec -> user

    //  obj id 별 객체를 생성할 필요가 굳이 없을것 같긴한데... 추후에 불변객체 생성 방식이 아닌 객체 변경 형태로 변경될 것을
    //  감안하여 현 상태를 유지한다.
    //
    //  todo: Discover를 위해 객체 생성을 담당하는 객체(User Manager)에서 NOM schema 에 대한 Sorted ListMap 을 만들고
    //        이를 통해 객체를 일정한 방식으로 생성해야 한다. (직렬화 정보 동기화)
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      val obj = SimulationManager.DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> Proto_NOMParser.getDefaultNOMSerializable(msg.name))))
        case None => Map(msg.name -> Map(msg.objID -> Proto_NOMParser.getDefaultNOMSerializable(msg.name)))
      }

      SimulationManager.DiscoverMap = SimulationManager.DiscoverMap ++ obj

    case ReflectMsg(msg) =>
      println("[Simulation Manager] Reflect msg received. " + msg)
      val obj = SimulationManager.DiscoverMap(msg.name)
        .updated(msg.objID, Proto_NOMParser
          .nomObjectTypeDeserializer(SimulationManager.DiscoverMap(msg.name)(msg.objID), msg.data))

      SimulationManager.DiscoverMap = SimulationManager.DiscoverMap ++ Map(msg.name -> obj)
      println(obj)

    case RecvMsg(msg) =>
      println("[Simulation Manager] Recv msg received. " + msg)
      val event = Proto_NOMParser.nomInteractionTypeDeserializer(Proto_NOMParser.getDefaultNOMSerializable(msg.name), msg.data)
      println(event)

      //  test code
      if (msg.name == "StartResume") doFlight()

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
