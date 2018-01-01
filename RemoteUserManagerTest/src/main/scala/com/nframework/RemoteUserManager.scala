package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props, Timers}
import com.nframework.Serializer._
import com.nframework.Serializer.NomSerializer._
import com.nframework.ControlManager.{DiscoverMap, TickKey, Update}
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

  var DiscoverMap = Map.empty[String, (NomSerializable, Int /*reference count*/ )]
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
    mec ! SendMsg(NMessage("StartResume", nomInteractionTypeSerializer(StartResume(20000))))

    Thread.sleep(100)  /// Simulation Manager 가 start event 처리를 하기 위한 시간 확보

    mec ! RegisterMsg("PowerOn", 1, managerName)
    mec ! RegisterMsg("PowerOn", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage("PowerOn", nomObjectTypeSerializer(PowerOn(1, updateValue + 1))))
      mec ! UpdateMsg(NMessage("PowerOn", nomObjectTypeSerializer(PowerOn(2, updateValue + 10001))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("PowerOn", Array[Byte]()))
      mec ! DeleteMsg(NMessage("PowerOn", Array[Byte]()))
    }

    updateValue += 1
  }


  def receive = {
    //  mec -> user

    //  Discover를 위해 객체 생성을 담당하는 객체(User Manager)에서 NOM schema 를 이용하여 인스턴스를 일관성 있게 생성해야 한다.

    //  실제 통신에 사용되는 object instance 가 다수일지라도 대응하는 object type 은 하나이며, 객체 간 구분은 해당 객체 class 에서
    //  필요로 하는 ID 를 정의해야 한다.
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      val obj = DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x._1, x._2 + 1))
        case None => Map(msg.name -> (getDefaultNOMSerializable(msg.name), 1))
      }
      DiscoverMap = DiscoverMap ++ obj


    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      val obj = nomObjectTypeDeserializer(DiscoverMap(msg.name)._1, msg.data)
      println(obj)


    case RecvMsg(msg) =>
      println("[Control Manager] Recv msg received. " + msg)
      val event = nomInteractionTypeDeserializer(getDefaultNOMSerializable(msg.name), msg.data)
      println(event)


    case RemoveMsg(msg) =>
      println("[Control Manager] Remove msg received. " + msg)
      val obj = DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x._1, x._2 - 1))
        case None => println("No Discovered msg. RemoveMsg request fail! msg name => " + msg.name); Nil
      }
      DiscoverMap = DiscoverMap ++ obj
      DiscoverMap = DiscoverMap.filter(_._2._2 != 0)
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
