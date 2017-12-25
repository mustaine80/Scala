package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.SimulationManager.{TickKey, Update}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NomSerializable, _}
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._

//  todo: override method 에 대한 NomSerializable trait 을 구현 mix-in 해야 한다.
case class Flight(id: Int, velocity: Double, position: Double) extends NomSerializable {
  override def getName(): String = "flight"

  override def getValues(): List[NValueType] =
    List(NInteger(id), NDouble(velocity), NDouble(position))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _id :: _velocity :: _position :: Nil => Flight(_id.toInt(), _velocity.toDouble(), _position.toDouble())
    case _ => println("[CLASS Flight] unknwon sequence... setValues() fail!"); Flight(0, 0.0, 0.0)
  }
}

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable {
  override def getName(): String = "powerOn"

  override def getValues(): List[NValueType] =
    List(NInteger(systemID), NInteger(subsystemID))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _systemID :: _subsystemID :: Nil => PowerOn(_systemID.toInt(), _subsystemID.toInt())
    case _ => println("[CLASS PowerOn] unknwon sequence... setValues() fail!"); PowerOn(0, 0)
  }
}

case class StartResume(isStart: Int)


object SimulationManager {
  private object TickKey
  private object Update

  var DiscoverMap = Map.empty[String, Map[Int, AnyRef]]


  /**
    *  NOM schema 를 이용하여 자동화. 일단 Object Type 만 적용한다.
    */

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

  Proto_NOMParser.parse("src/main/Resources/test.json")
  val objects = Proto_NOMParser.objectTypes
  val interactions = Proto_NOMParser.interactionTypes


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

  /**
    * todo: update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달할 수 있는 기능이 필요하다.
    *       32bit flag ??
    */
  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage(
        "flight", 1, Proto_NOMParser.nomObjectTypeSerializer(Flight(1, updateValue * 10.0, updateValue * 50.0))))

      mec ! UpdateMsg(NMessage(
        "flight", 2, Proto_NOMParser.nomObjectTypeSerializer(Flight(2, updateValue * 20.0, updateValue * 100.0))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("flight", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("flight", 2, Array[Byte]()))
    }

    updateValue += 1
  }

  def procStartResume(event: StartResume): Unit = doFlight()

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
          val m = SimulationManager.DiscoverMap(msg.name).
            updated(msg.objID, Proto_NOMParser.nomObjectTypeDeserializer(PowerOn(0,0), msg.data))
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
