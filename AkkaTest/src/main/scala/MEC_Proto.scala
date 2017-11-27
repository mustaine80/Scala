import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}

import scala.collection.mutable.ListBuffer

private object Start

object MEC_Proto {
  private object Pop
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
case class ReflectMsg(msg: NOM, buffer: Byte)
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

  def deleteMsg(nomMsg: NOM): Unit = {
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

  //  todo: need to merge type parameter method of "msgPop[T]"
  def popReflectedMsg(xs: ListBuffer[(NOM, Byte)]): ListBuffer[(NOM, Byte)] = {
    xs match {
      case ListBuffer() => xs
      case x +: xsLeft =>
        user ! ReflectMsg(x._1, x._2)
        popReflectedMsg(xsLeft)
    }
  }


  def msgPop[T](xs: ListBuffer[T], proc: T => Unit): ListBuffer[T] = {
    xs match {
      case ListBuffer() => xs
      case x +: xsLeft =>
        proc(x)
        msgPop(xsLeft, proc)
    }
  }

  def task(): Unit = {
    println("NOM msg processing...")

    receivedNOMList = msgPop(receivedNOMList, user ! RecvMsg(_))
    discoveredNOMList = msgPop(discoveredNOMList, user ! DiscoverMsg(_))
    removedNOMList = msgPop(removedNOMList, user ! RemoveMsg(_))

    //  todo: need to replace msgPop()
    reflectedNOMList = popReflectedMsg(reflectedNOMList)
  }

  def receive = {
    //  mec initialize
    case Start => timers.startPeriodicTimer(TickKey, Pop, 1.second)
    case ActorRef => setMEB(_)

    //  user -> mec: data request
    case name: String => registerMsg(name)
    case UpdateMsg => updateMsg(_)
    case DeleteMsg => deleteMsg(_)
    case SendMsg => sendMsg(_)

    //  meb -> mec: data push
    case (msg: NOM, buf: Byte) => reflectMsg(msg, buf)
    case DiscoverMsg => discoverMsg(_)
    case RecvMsg => recvMsg(_)
    case RemoveMsg => removeMsg(_)

    //  mec self scheduling
    case Pop => task()
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