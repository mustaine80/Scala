package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.SimulationManager.{TickKey, Update}
import com.nframework.meb.MEB_Proto
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NomSerializable, _}
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._


//  override method 에 대한 NomSerializable trait 에 대한 구현 mix-in 은 getName() 만을 제공한다.
//  getValues(), setValues(), 보조 생성자 메소드는 사용자가 구현한다.
case class Flight(id: Int, velocity: Double, position: Double) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(id), NDouble(velocity), NDouble(position))


  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _id :: _velocity :: _position :: Nil => Flight(_id.toInt(), _velocity.toDouble(), _position.toDouble())
    case _ => println("[CLASS Flight] unknwon sequence... setValues() fail!"); Flight(0, 0.0, 0.0)
  }

  def this() { this(id = 0, velocity = 0.0, position = 0.0) }
}

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(systemID), NInteger(subsystemID))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _systemID :: _subsystemID :: Nil => PowerOn(_systemID.toInt(), _subsystemID.toInt())
    case _ => println("[CLASS PowerOn] unknwon sequence... setValues() fail!"); PowerOn(0, 0)
  }

  def this() { this(systemID = 0, subsystemID = 0) }
}

case class StartResume(isStart: Int) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(isStart))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _isStart :: Nil => StartResume(_isStart.toInt())
    case _ => println("[CLASS StartResume] unknwon sequence... setValues() fail!"); StartResume(0)
  }

  //  todo: NomSerializable 에서 abstract method 로 선언해야 한다.
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
    println("!!@#@!#!!!" + getNOMTemplate("StartResume"))
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
        "Flight", 1, Proto_NOMParser.nomObjectTypeSerializer(Flight(1, updateValue * 10.0, updateValue * 50.0))))

      mec ! UpdateMsg(NMessage(
        "Flight", 2, Proto_NOMParser.nomObjectTypeSerializer(Flight(2, updateValue * 20.0, updateValue * 100.0))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("Flight", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("Flight", 2, Array[Byte]()))
    }

    updateValue += 1
  }

  //  msg name 으로 기본 객체를 반환한다.
  def getNOMTemplate(msgName: String): NomSerializable = {
    val nom = Class.forName("com.nframework." + msgName).newInstance()
    nom.asInstanceOf[NomSerializable].getDefault
  }


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
        .updated(msg.objID, Proto_NOMParser.nomObjectTypeDeserializer(getNOMTemplate(msg.name), msg.data))

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
