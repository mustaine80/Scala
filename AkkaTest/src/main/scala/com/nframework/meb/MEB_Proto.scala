package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.MebAttatch
import com.nframework.mec._
import com.nframework.nom.{NChar, NOM, NValueType}

import scala.collection.mutable

case class DummyNOM(objName: String, value: NValueType) extends NOM

class MEB_Proto extends Actor {
  val mecMap = mutable.Map[String, ActorRef]()

  init()

  def init(): Unit = {
    println("MEB initiaize ...")
  }


  //  todo: need to implement
  def receive = {
    case MebAttatch(name) =>
      mecMap(name) = sender()

    case t @ RegisterMsg(msgName, userName) =>
      println("[MEB] msg register: " + msgName + ", " + userName)
      val dummy = DummyNOM(msgName, NChar('a'))
      sender() ! DiscoverMsg(dummy)   /// Seq[ActorRef](Subscribers...).foreach.{_ ! DicoverMsg}

    case t @ UpdateMsg(nomMsg) => sender() ! t

    case t @ SendMsg(nomMsg) => sender() ! t

    case t @ DeleteMsg(nomMsg) => sender() ! t
  }
}
