package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props, Timers}
import com.nframework.ControlManager.{TickKey, Update}
import com.nframework.mec.MEC_Proto.PubSubInfoForwarding
import com.nframework.mec._
import com.nframework.nom.{NDouble, NInteger, _}
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._


//  override method 에 대한 NomSerializable trait 에 대한 구현 mix-in 은 getName() 만을 제공한다.
//  getValues(), setValues() 메소드는 사용자가 구현한다.
case class Flight(id: Int, velocity: Double, position: Double) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(id), NDouble(velocity), NDouble(position))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _id :: _velocity :: _position :: Nil => Flight(_id.toInt(), _velocity.toDouble(), _position.toDouble())
    case _ => println("[CLASS Flight] unknwon sequence... setValues() fail!"); Flight(0, 0.0, 0.0)
  }
}

case class PowerOn(systemID: Int, subsystemID: Int) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(systemID), NInteger(subsystemID))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _systemID :: _subsystemID :: Nil => PowerOn(_systemID.toInt(), _subsystemID.toInt())
    case _ => println("[CLASS PowerOn] unknwon sequence... setValues() fail!"); PowerOn(0, 0)
  }
}

case class StartResume(isStart: Int) extends NomSerializable {
  override def getValues(): List[NValueType] =
    List(NInteger(isStart))

  override def setValues(ns: NValueType*): NomSerializable = ns match {
    case _isStart :: Nil => StartResume(_isStart.toInt())
    case _ => println("[CLASS StartResume] unknwon sequence... setValues() fail!"); StartResume(0)
  }
}


object ControlManager {
  private object TickKey
  private object Update

  var DiscoverMap = Map.empty[String, Map[Int, NomSerializable]]
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
    mec ! SendMsg(NMessage("StartResume", 0, Proto_NOMParser.nomInteractionTypeSerializer(StartResume(20000))))

    Thread.sleep(100)  /// Simulation Manager 가 start event 처리를 하기 위한 시간 확보

    mec ! RegisterMsg("PowerOn", 1, managerName)
    mec ! RegisterMsg("PowerOn", 2, managerName)

    timers.startPeriodicTimer(TickKey, Update, 10.millisecond)
  }

  def update(): Unit = {
    if (updateValue < 1000) {
      mec ! UpdateMsg(NMessage("PowerOn", 1, Proto_NOMParser.nomObjectTypeSerializer(PowerOn(1, updateValue + 1))))
      mec ! UpdateMsg(NMessage("PowerOn", 2, Proto_NOMParser.nomObjectTypeSerializer(PowerOn(2, updateValue + 10001))))
    }

    if (updateValue == 1000) {
      mec ! DeleteMsg(NMessage("PowerOn", 1, Array[Byte]()))
      mec ! DeleteMsg(NMessage("PowerOn", 2, Array[Byte]()))
    }

    updateValue += 1
  }

  //  todo: 내가 구독한 메시지에 대해 사용자가 정의해야 한다. 자동화할 필요가 있다.
  def getNOMTemplate(msgName: String): NomSerializable = {
    msgName match {
      case "Flight" => Flight(0, 0.0, 0.0)
      case "PowerOn" => PowerOn(0, 0)
      case "StartResume" => StartResume(0)

      case _ => println("[Control Manager] msg is not own subscription. " + msgName); StartResume(0)
    }
  }


  def receive = {
    //  mec -> user
    //  discover, reflect, remove 를 위한 case class 를 수작업으로 매칭하는 것은 receive 함수가 벌크해지기 때문에 추출한다.
    case DiscoverMsg(msg) =>
      println("[Control Manager] discover msg received. " + msg)
      val obj = ControlManager.DiscoverMap.get(msg.name) match {
        case Some(x) => Map(msg.name -> (x ++ Map(msg.objID -> getNOMTemplate(msg.name))))
        case None => Map(msg.name -> Map(msg.objID -> getNOMTemplate(msg.name)))
      }

      ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ obj

    case ReflectMsg(msg) =>
      println("[Control Manager] Reflect msg received. " + msg)
      val obj = ControlManager.DiscoverMap(msg.name)
        .updated(msg.objID, Proto_NOMParser.nomObjectTypeDeserializer(getNOMTemplate(msg.name), msg.data))

      ControlManager.DiscoverMap = ControlManager.DiscoverMap ++ Map(msg.name -> obj)
      println(obj)

    case RecvMsg(msg) =>
      println("[Control Manager] Recv msg received. " + msg)
      val event = Proto_NOMParser.nomInteractionTypeDeserializer(getNOMTemplate(msg.name), msg.data)
      println(event)

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
