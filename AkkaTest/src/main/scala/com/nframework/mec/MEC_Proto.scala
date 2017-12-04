package com.nframework.mec

import akka.actor.{Actor, ActorRef, Props, Timers}
import com.nframework.mec.MEC_Proto._
import com.nframework.nom._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._


object MEC_Proto {
  private object Pop
  private object TickKey

  case class MebAttatch(name: String)
  case class GetState(test: ActorRef) /// only test

  def props(test: ActorRef) = {
    Props(new MEC_Proto("MecTestActor", null, test))
  }
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

class MEC_Proto(userName: String, user: ActorRef, meb: ActorRef)
  extends Actor with Timers {

  var receivedNOMList = ListBuffer[RecvMsg]()
  var reflectedNOMList = ListBuffer[(ReflectMsg)]()
  var discoveredNOMList = ListBuffer[DiscoverMsg]()
  var removedNOMList = ListBuffer[RemoveMsg]()

  init()

  def init(): Unit = {
    meb ! MebAttatch(userName)  /// meb receiver 에서 MebAttatch 에 대한 sender()를 통해 ref 획득
    timers.startPeriodicTimer(TickKey, Pop, 1.second)
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

    discoveredNOMList = msgPop(discoveredNOMList, user ! _)
    reflectedNOMList = msgPop(reflectedNOMList, user ! _)
    receivedNOMList = msgPop(receivedNOMList, user ! _)
    removedNOMList = msgPop(removedNOMList, user ! _)
  }

  def receive = {
    //  user -> mec: data request
    case t @ RegisterMsg(msgName, userName) => meb ! t

    case t @ UpdateMsg(nomMsg) => meb ! t

    case t @ SendMsg(nomMsg) => meb ! t

    case t @ DeleteMsg(nomMsg) => meb ! t


    //  meb -> mec: data push
    case s @ DiscoverMsg(msg) => discoveredNOMList += s

    case s @ ReflectMsg(msg, buf) => reflectedNOMList += s

    case s @ RecvMsg(msg) => {
      //  val msg = nomMsg.clone()
      //  msg.setOwner(userName)
      receivedNOMList += s
    }

    case s @ RemoveMsg(msg) => {
      val r = (discoveredNOMList find (_.msg == s.msg)).get
      discoveredNOMList -= r
      removedNOMList += RemoveMsg(r.msg)
    }

    //  mec self scheduling
    case Pop => task()

    //  using akka-TestKit
    case GetState(test) => {
      test ! discoveredNOMList
      test ! reflectedNOMList
      test ! receivedNOMList
      test ! removedNOMList
    }

    // unknown message
    case m: String => println(s"received $m")
  }
}

//  UserManager as network backend. create MEC instance
//object MEC_Test {
//  def main(args: Array[String]): Unit = {
//    val config = ConfigFactory.load("server")
//    val system = ActorSystem("server", config)
//
//    val mecSurrogate: ActorRef = null
//    val meb: ActorRef = null
//
//    val mec = system.actorOf(Props(new MEC_Proto("MEC", mecSurrogate, meb)), "mec")
//
//    mec ! Start
//  }
//}