package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.Serializer._
import com.nframework.Serializer.NomSerializer._
import com.nframework.SimulationManager.{DiscoverMap, TickKey, Update}
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

  var DiscoverMap = Map.empty[String, (NomSerializable, Int /*reference count*/ )]

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

  //  todo: update 시 full object 가 아닌 실제 변경이 일어난 부분 정보만 전달할 수 있는 기능이 필요하다. (32bit flag)
  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage(
        "Flight", nomObjectTypeSerializer(Flight(1, updateValue * 10.0,
          Position(updateValue * 50.0, updateValue * 30.0, updateValue * 10.0)))))

      mec ! UpdateMsg(NMessage(
        "Flight", nomObjectTypeSerializer(Flight(2, updateValue * 20.0,
          Position(updateValue * 100.0, updateValue * 50.0, updateValue * 20.0)))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("Flight", Array[Byte]()))
      mec ! DeleteMsg(NMessage("Flight", Array[Byte]()))
    }

    updateValue += 1
  }


  def receive = {
    //  mec -> user

    //  Discover를 위해 객체 생성을 담당하는 객체(User Manager)에서 NOM schema 를 이용하여 인스턴스를 일관성 있게 생성해야 한다.

    //  실제 통신에 사용되는 object instance 가 다수일지라도 대응하는 object type 은 하나이며, 객체 간 구분은 해당 객체 class 에서
    //  필요로 하는 ID 를 정의해야 한다.
    case DiscoverMsg(msg) =>
      println("[Simulation Manager] discover msg received. " + msg)
      val obj = DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x._1, x._2 + 1))
        case None => Map(msg.name -> (getDefaultNOMSerializable(msg.name), 1))
      }
      DiscoverMap = DiscoverMap ++ obj


    case ReflectMsg(msg) =>
      println("[Simulation Manager] Reflect msg received. " + msg)
      val obj = nomObjectTypeDeserializer(DiscoverMap(msg.name)._1, msg.data)
      println(obj)


    case RecvMsg(msg) =>
      println("[Simulation Manager] Recv msg received. " + msg)
      val event = nomInteractionTypeDeserializer(getDefaultNOMSerializable(msg.name), msg.data)
      println(event)
      //  test code
      if (msg.name == "StartResume") doFlight()


    case RemoveMsg(msg) =>
      println("[Simulation Manager] Remove msg received. " + msg)
      val obj = DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x._1, x._2 - 1))
        case None => println("No Discovered msg. RemoveMsg request fail! msg name => " + msg.name); Nil
      }
      DiscoverMap = DiscoverMap ++ obj
      DiscoverMap = DiscoverMap.filter(_._2._2 != 0)
      println(DiscoverMap)


    case Update => update()


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
