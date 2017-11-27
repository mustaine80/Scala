import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}

import scala.collection.mutable.ListBuffer

private object Start

object MEC_Proto {
  private object Pop
  private object Tick
  private object TickKey
}

class NOM

/// msg: meb -> mec
case class RegisterMsg(msgName: String, userName: String)
case class UpdateMsg(msg: NOM)
case class DeleteMsg(msg: NOM)
case class SendMsg(msg: NOM)


/// msg: mec -> meb
case class RecvMsg(msg: NOM)
case class ReflectMsg(msg: NOM)
case class DiscoverMsg(msg: NOM)
case class RemoveMsg(msg: NOM)


class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef) extends Actor with Timers {
  import MEC_Proto._

  var receivedNOMList = ListBuffer[NOM]()
  var reflectedNOMList = ListBuffer[(NOM, Byte)]()
  var discoveredNOMList = ListBuffer[NOM]()
  var removedNOMList = ListBuffer[NOM]()

  def setMEB(mec: ActorRef): Unit = {
    meb ! mec   /// attach reference
  }

  def registerMsg(msgName: String): Unit = {
    meb ! RegisterMsg(msgName, userName)
  }

  def discoverMsg(nomMsg: NOM): Unit = {
    discoveredNOMList += nomMsg
  }

  def updateMsg(nomMsg: NOM): Unit = {
    meb ! UpdateMsg(nomMsg)
  }

  def reflectMsg(nomMsg: NOM, buffer: Byte): Unit = {
    reflectedNOMList += ((nomMsg, buffer))
  }

  def deleteMSg(nomMsg: NOM): Unit = {
    meb ! DeleteMsg(nomMsg)
  }

  def removeMsg(nomMsg: NOM): Unit = {
    val r = (discoveredNOMList find (x => x == nomMsg)).get
    discoveredNOMList -= r
    removedNOMList += r
  }

  def sendMsg(nomMsg: NOM): Unit = {
    meb ! SendMsg(nomMsg)
  }

  def recvMsg(nomMsg: NOM): Unit = {
    //  val msg = nomMsg.clone()  /// ???
    //  msg.setOwner(userName)

    receivedNOMList += nomMsg
  }

  def msgPop(): Unit = {
    println("NOM msg processing...")

    receivedNOMList.foreach(user ! RemoveMsg(_))
    reflectedNOMList.foreach {
      case x: (NOM, Byte) => user ! ReflectMsg(x._1)
      case _ =>
    }
    discoveredNOMList.foreach(user ! DiscoverMsg(_))
    removedNOMList.foreach(user ! RemoveMsg(_))

    receivedNOMList = ListBuffer[NOM]()
    reflectedNOMList = ListBuffer[(NOM, Byte)]()
    discoveredNOMList = ListBuffer[NOM]()
    removedNOMList = ListBuffer[NOM]()
  }

  def receive = {
    case Start => timers.startPeriodicTimer(TickKey, Pop, 1.second)
    case Pop => msgPop()
  }
}


object MEC_Test {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("nFrameworkActorSystem")

    val mecSurrogate: ActorRef = null
    val meb: ActorRef = null

    val mec = system.actorOf(Props(new MEC_Proto("CommunicationManager", mecSurrogate, meb)))

    mec ! Start
  }
}