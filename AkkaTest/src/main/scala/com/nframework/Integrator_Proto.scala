package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.nframework.meb.MEB_Proto
import com.nframework.mec._
import com.typesafe.config.ConfigFactory

class SimulationManager(meb: ActorRef) extends Actor {
  val mec = context.actorOf(Props(new MEC_Proto("SimulationManager", context.self, meb)), "MEC_SimulationManager")
  val managerName = "Simulation Manager"

  init()

  def init(): Unit = {
    println("simulation manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요
    mec ! RegisterMsg("object1", managerName)
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    case DiscoverMsg(msg) => println("[Simulation Manager] discover msg received. " + msg)
    case ReflectMsg(msg, buf) =>
    case RecvMsg(msg) =>
    case RemoveMsg(msg) =>
  }
}

object Integrator_Test {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("server")
    val system = ActorSystem("server", config)

    //  todo: need to create integrator level actor?
    val meb = system.actorOf(Props[MEB_Proto], "MEB")
    val simulationManager = system.actorOf(Props(new SimulationManager(meb)), "SimulationManager")
  }
}
