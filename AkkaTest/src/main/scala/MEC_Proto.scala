import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable.ListBuffer

private object Start

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


class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef) extends Actor {
  var receivedNOMList = ListBuffer[NOM]()
  var reflectedNOMList = ListBuffer[(NOM, Byte)]()
  var discoveredNOMList = ListBuffer[NOM]()
  var removedNOMList = ListBuffer[NOM]()

  def initActor(): Unit = {
    //  todo: use akka scheduler
    for (i <- 1 to 5) {
      println(i + "th NOM msg processing...")
      msgPop()
      Thread.sleep(1000)
    }
  }

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

  //  todo: code duplicaton
  def popReceivedMsg(xs: List[NOM]): List[NOM] = {
    xs match {
      case Nil => xs
      case x :: xsLeft =>
        user ! RecvMsg(x)
        popReceivedMsg(xsLeft)
    }
  }

  def popReflectedMsg(xs: List[(NOM, Byte)]): List[(NOM, Byte)] = {
    xs match {
      case Nil => xs
      case x :: xsLeft =>
        user ! ReflectMsg(x._1)
        popReflectedMsg(xsLeft)
    }
  }

  def popDiscoveredMsg(xs: List[NOM]): List[NOM] = {
    xs match {
      case Nil => xs
      case x :: xsLeft =>
        user ! DiscoverMsg(x)
        popDiscoveredMsg(xsLeft)
    }
  }

  def popRemovedMsg(xs: List[NOM]): List[NOM] = {
    xs match {
      case Nil => xs
      case x :: xsLeft =>
        user ! RemoveMsg(x)
        popRemovedMsg(xsLeft)
    }
  }

  def msgPop(): Unit = {
    popReceivedMsg(receivedNOMList.toList)
    popReflectedMsg(reflectedNOMList.toList)
    popDiscoveredMsg(discoveredNOMList.toList)
    popRemovedMsg(removedNOMList.toList)

    receivedNOMList = ListBuffer[NOM]()
    reflectedNOMList = ListBuffer[(NOM, Byte)]()
    discoveredNOMList = ListBuffer[NOM]()
    removedNOMList = ListBuffer[NOM]()
  }

  def receive = {
    case Start => initActor()
  }
}


object MEC_Test {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("nFrameworkActorSystem")

    val mecSurrogate: ActorRef = null
    val meb: ActorRef = null

    val mec = system.actorOf(Props(new MEC_Proto("CommunicationManager", mecSurrogate, meb)))

    mec ! Start
    system.terminate()
  }
}