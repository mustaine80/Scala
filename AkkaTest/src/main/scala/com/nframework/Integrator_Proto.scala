package com.nframework

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.nframework.meb.MEB_Proto
import com.nframework.mec._
import com.nframework.nom._
import com.typesafe.config.ConfigFactory


object SimulationManager {
  /** todo: NOMParser 의 parsing 정보는 1회만 생성하고 이를 가져다 쓰는 방식이어야 하는데, 현재는 필요시 생성하여 쓰는 방식이다.
    * immutable 하게 생성하여 get 하는 방식으로 보완해야 한다.
   */
  NOMParser.parse("src/main/Resources/test.json")
  val objects = Proto_NOMParser.objectTypes
  val interactions = Proto_NOMParser.interactionTypes
}

class SimulationManager(meb: ActorRef) extends Actor {
  val mec = context.actorOf(Props(new MEC_Proto("SimulationManager", context.self, meb)), "MEC_SimulationManager")
  val managerName = "Simulation Manager"

  init()

  def init(): Unit = {
    println("simulation manager initialize ...")
    Thread.sleep(5000)    //  todo: 현재 MEB 에 msg sharing 정보 전파 이후에 RegisterMsg 요청이 가능함. 보완 필요 (Future?)
    doFlight()
  }


  /** todo: NOM Parser 에 있는 객체 모델을 이용하여 NOM 템플릿을 제공한다.
    * Manager 는 템플릿 객체를 받아와서 자신의 Pub 정보를 기술해야 한다.
   */
  def getNOMTemplate(template: String): List[NOM] = {
    SimulationManager.objects.get(template) match {
      case Some(x) => DummyHead(template) :: x.fields.map{
        case Field_Proto(name, model, size, fixedLength, indicator) =>
          (DummyNOM(name, NChar_Dummy('S'))) }.toList   /// todo: 일단 임시로 dummy 하게 만들어 본다. 아래처럼 구현해야 한다.
          //  NType(name) :: NType(model) * size in loop :: fixedLength(TBD) :: indicator(TBD)
      case None => println("[Manager] requested object NOM template is not available."); Nil
      }
  }


  /** test code

   */
  def doFlight(): Unit = {
    /** getNOMTemplate 를 통해 받아온 템플릿 객체에 flight 정보를 입력해야 한다. 테스트 코드이기 때문에 이대로 유지한다.
     */
    val flight1 = getNOMTemplate("object1")
    mec ! RegisterMsg(flight1(0).getName, managerName)
    mec ! UpdateMsg(flight1)

    val flight2 = getNOMTemplate("object2")
    mec ! RegisterMsg(flight2(0).getName, managerName)
    mec ! UpdateMsg(flight2)
  }

  //  todo: need to implement
  def receive = {
    //  mec -> user
    case DiscoverMsg(msg) => println("[Simulation Manager] discover msg received. " + msg)
    case ReflectMsg(msg) =>
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
