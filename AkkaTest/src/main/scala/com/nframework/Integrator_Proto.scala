package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.SimulationManager.{TickKey, Update}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NomSerializable, _}
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._


//  !!  Enum/Complext type 에 대해 basic type 단위의 NValue 구성을 사용자가 작성한다.
case class Position(x: Double, y: Double, z: Double) extends NomSerializable {
  def this() { this(x = 0.0, y = 0.0, z = 0.0)}
}

case class Flight(id: Int, velocity: Double, position: Position) extends NomSerializable {
  def this() { this(id = 0, velocity = 0.0, position = Position(0.0, 0.0, 0.0)) }
}

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable {
  def this() { this(systemID = 0, subsystemID = 0) }
}

case class StartResume(isStart: Int) extends NomSerializable {
  def this() { this(isStart = 0) }
}


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

  //  msg name 으로 기본 객체를 반환한다.
  def getNOMTemplate(msgName: String): NomSerializable =
    Class.forName("com.nframework." + msgName).newInstance().asInstanceOf[NomSerializable]



  def receive = {
    //  mec -> user
    //  discover, reflect, remove 를 위한 case class 를 수작업으로 매칭하는 것은 receive 함수가 벌크해지기 때문에 추출한다.
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      val obj = SimulationManager.DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> getNOMTemplate(msg.name))))
        case None => Map(msg.name -> Map(msg.objID -> getNOMTemplate(msg.name)))
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
      val event = Proto_NOMParser.nomInteractionTypeDeserializer(getNOMTemplate(msg.name), msg.data)
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
