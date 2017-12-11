package com.nframework.meb

import akka.actor.{Actor, ActorRef}
import com.nframework.mec.MEC_Proto.MebAttatch
import com.nframework.mec._
import com.nframework.nom
import com.nframework.nom.NChar_Dummy

import scala.collection.mutable


//  todo: remote actor msg need to seriaize. don't use Java basic serialization
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
      println("[MEB] register msg received. : " + msgName + ", from : " + userName)
      val dummy = nom.DummyNOM(msgName, NChar_Dummy('a'))
      sender() ! DiscoverMsg(dummy)   /// todo: Seq[ActorRef](Subscribers...).foreach.{_ ! DicoverMsg}

    case t @ UpdateMsg(nomMsg) => sender() ! t

    case t @ SendMsg(nomMsg) => sender() ! t

    case t @ DeleteMsg(nomMsg) => sender() ! t
  }
}
