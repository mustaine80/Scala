package com.nframework.mec

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Timers}
import com.nframework.mec.MEC_Proto.{GetState, Pop, SetMEB, TickKey}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import com.nframework.nom._

private object Start

object MEC_Proto {
  private object Pop
  private object TickKey

  case class SetMEB(sender: ActorRef)
  case class GetState(receiver: ActorRef)
}

abstract class PubSub

/// msg: mec -> meb
case class RegisterMsg(msgName: String, userName: String) extends PubSub
case class UpdateMsg(msg: NOM) extends PubSub
case class SendMsg(msg: NOM) extends PubSub
case class DeleteMsg(msg: NOM) extends PubSub


/// msg: meb -> mec
case class ReflectMsg(msg: NOM, buffer: Byte) extends PubSub
case class DiscoverMsg(msg: NOM) extends PubSub
case class RecvMsg(msg: NOM) extends PubSub
case class RemoveMsg(msg: NOM) extends PubSub

class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef) extends Actor with Timers {
  var receivedNOMList = ListBuffer[RecvMsg]()
  var reflectedNOMList = ListBuffer[(ReflectMsg)]()
  var discoveredNOMList = ListBuffer[DiscoverMsg]()
  var removedNOMList = ListBuffer[RemoveMsg]()

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

    discoveredNOMList = msgPop(discoveredNOMList, user ! _)
    reflectedNOMList = msgPop(reflectedNOMList, user ! _)
    receivedNOMList = msgPop(receivedNOMList, user ! _)
    removedNOMList = msgPop(removedNOMList, user ! _)
  }

  def receive = {
    //  mec initialize
    case Start => timers.startPeriodicTimer(TickKey, Pop, 1.second)

    case mec @ SetMEB(receiver: ActorRef) => meb ! mec

    //  user -> mec: data request
    case t @ RegisterMsg(msgName: String, userName: String) => meb ! t

    case t @ UpdateMsg(nomMsg: NOM) => meb ! t

    case t @ SendMsg(nomMsg: NOM) => meb ! t

    case t @ DeleteMsg(nomMsg: NOM) => meb ! t


    //  meb -> mec: data push
    case s @ DiscoverMsg(msg: NOM) => discoveredNOMList += s

    case s @ ReflectMsg(msg: NOM, buf: Byte) => reflectedNOMList += s

    case s @ RecvMsg(msg: NOM) => {
      //  val msg = nomMsg.clone()
      //  msg.setOwner(userName)
      receivedNOMList += s
    }

    case s @ RemoveMsg(msg: NOM) => {
      val r = (discoveredNOMList find (_ == s)).get
      discoveredNOMList -= r
      removedNOMList += RemoveMsg(r.msg)
    }

    //  mec self scheduling
    case Pop => task()

    //  using akka-TestKit
    case GetState(receiver) => {
      receiver ! discoveredNOMList
      receiver ! reflectedNOMList
      receiver ! receivedNOMList
//      receiver ! removedNOMList
    }

    // unknown message
    case m: String => println(s"received $m")
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