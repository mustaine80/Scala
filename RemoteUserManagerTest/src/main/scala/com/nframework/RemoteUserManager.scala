package com.nframework

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props}
import com.typesafe.config.ConfigFactory
import com.nframework.mec._

class ControlManager(meb: ActorRef) extends Actor {
  val mec = context.actorOf(Props(new MEC_Proto("ControlManager", context.self, meb)), "MEC_ControlManager")
  val managerName = "Control Manager"

  init()

  def init(): Unit = {
    println("control manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요
    mec ! RegisterMsg("object2", managerName)
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    //  todo: need to NOM serialization
    case DiscoverMsg(msg) => println("[Control Manager] discover msg received. " + msg)
    case ReflectMsg(msg, buf) =>
    case RecvMsg(msg) =>
    case RemoveMsg(msg) =>
  }
}


class RemoteUserManager_Test(selection: ActorSelection, path: String) extends Actor {
  selection ! Identify(None)

  //  todo: MEB actor that do not work should be considered
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
