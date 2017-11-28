import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

private object Start

object MEC_Proto {
  private object Pop
  private object TickKey
}

class NOM

class PubSub

/// msg: meb -> mec
case class RegisterMsg(msgName: String, userName: String) extends PubSub
case class UpdateMsg(msg: NOM) extends PubSub
case class DeleteMsg(msg: NOM) extends PubSub
case class SendMsg(msg: NOM) extends PubSub


/// msg: mec -> meb
case class RecvMsg(msg: NOM) extends PubSub
case class ReflectMsg(msg: NOM, buffer: Byte) extends PubSub
case class DiscoverMsg(msg: NOM) extends PubSub
case class RemoveMsg(msg: NOM) extends PubSub

class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef) extends Actor with Timers {
  import MEC_Proto._

  var receivedNOMList = ListBuffer[RecvMsg]()
  var reflectedNOMList = ListBuffer[(ReflectMsg)]()
  var discoveredNOMList = ListBuffer[DiscoverMsg]()
  var removedNOMList = ListBuffer[RemoveMsg]()

  def setMEB(mec: ActorRef): Unit = {
    meb ! mec   /// attach reference
  }

  def registerMsg(msgName: String): Unit = {
    meb ! RegisterMsg(msgName, userName)
  }

  def discoverMsg(nomMsg: NOM): Unit = {
    discoveredNOMList += DiscoverMsg(nomMsg)
  }

  def updateMsg(nomMsg: NOM): Unit = {
    meb ! UpdateMsg(nomMsg)
  }

  def reflectMsg(nomMsg: NOM, buffer: Byte): Unit = {
    reflectedNOMList += ReflectMsg(nomMsg, buffer)
  }

  def deleteMsg(nomMsg: NOM): Unit = {
    meb ! DeleteMsg(nomMsg)
  }

  def removeMsg(nomMsg: NOM): Unit = {
    val r = (discoveredNOMList find (_ == DiscoverMsg(nomMsg))).get
    discoveredNOMList -= r
    removedNOMList += RemoveMsg(r.msg)
  }

  def sendMsg(nomMsg: NOM): Unit = {
    meb ! SendMsg(nomMsg)
  }

  def recvMsg(nomMsg: NOM): Unit = {
    //  val msg = nomMsg.clone()  /// ???
    //  msg.setOwner(userName)

    receivedNOMList += RecvMsg(nomMsg)
  }

  def msgPop[U <: PubSub](xs: ListBuffer[U], proc: U => Unit): ListBuffer[U] = {
    xs match {
      case ListBuffer() => xs
      case x +: xsLeft =>
        proc(x)
        msgPop(xsLeft, proc)
    }
  }
  
  def task(): Unit = {
    println("NOM msg processing...")

    receivedNOMList = msgPop(receivedNOMList, user ! _)
    reflectedNOMList = msgPop(reflectedNOMList, user ! _)
    discoveredNOMList = msgPop(discoveredNOMList, user ! _)
    removedNOMList = msgPop(removedNOMList, user ! _)
  }

  def receive = {
    // for client msg receiving test.
    case m: String => println(s"received $m")

    //  mec initialize
    case Start => timers.startPeriodicTimer(TickKey, Pop, 1.second)
    case ActorRef => setMEB(_)

    //  user -> mec: data request
    case RegisterMsg => registerMsg(_)
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

//  UserManager as network backend. create MEC instance
object MEC_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("server")
    val system = ActorSystem("server", config)

    val mecSurrogate: ActorRef = null
    val meb: ActorRef = null

    val mec = system.actorOf(Props(new MEC_Proto("MEC", mecSurrogate, meb)), "mec")

    mec ! Start
  }
}