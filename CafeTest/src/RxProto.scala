

/*
  reactive stream simply implementation
  protocol: onSubscribe onNext* (onError | onComplete)
 */


sealed trait Publisher {
  def subscribe(sub: Subscriber): Unit
}


sealed trait Subscriber {
  def onSubscribe(s: Subscription): Unit  //  todo: ????

  def onNext(t: Int): Unit    //  todo: generics

  def onError(t: Throwable): Unit

  def onComplete: Unit
}


sealed abstract class Subscription {
  def cancel: Unit

  def request(n: Int): Unit   //  todo: argument  Int => QoS ?
}


trait Processor extends Publisher with Subscriber {
  var _pub: Publisher = null
  var _sub: Subscriber = null
  var _s: Subscription = null

  def init(pub: Publisher, sub: Subscriber): Unit = { //  todo: how force to init
    _pub = pub
    _sub = sub
  }

  override def subscribe(sub: Subscriber): Unit = {
    _pub.subscribe(sub)
    _sub = sub
  }

  override def onSubscribe(s: Subscription): Unit = {
    _sub.onSubscribe(s)
    _s = s
  }

  override def onError(t: Throwable): Unit = println(t.printStackTrace())
}


case class PubAir() extends Publisher {
  val data = Stream.range(0, 100, 1)  //  data sampling

  var upperBound = Int.MaxValue   //  todo: var => val

  val subscription = new Subscription {
    override def cancel: Unit = println("cancel")

    override def request(n: Int): Unit = { upperBound = n }
  }

  override def subscribe(sub: Subscriber): Unit = {
    println("[Air] subscribe")

    sub.onSubscribe(subscription)
    data take upperBound foreach sub.onNext
    sub.onComplete
  }
}


case class SubSensor() extends Subscriber {
  override def onSubscribe(s: Subscription): Unit = {
    println("[Sensor] onSubscribe")
    s.request(10)
  }

  override def onNext(t: Int): Unit = println("[Sensor] onNext - " + t)

  override def onError(t: Throwable): Unit = println("[Sensor] onError: " + t.printStackTrace())

  override def onComplete: Unit = println("[Sensor] onComplete")
}


case class MapOperator() extends Processor {
  override def onNext(t: Int): Unit = {
    println("[map] onNext - " + t * 2)
    _sub.onNext(t * 2)
  }

  override def onComplete: Unit = {
    println("[map] onComplete")
    _sub.onComplete
  }
}


case class FoldOperator() extends Processor {
  import scala.collection.mutable.ListBuffer

  val data = ListBuffer.empty[Int]

  override def onNext(n: Int): Unit = {
    data += n
    println("[fold] onNext - " + data.foldRight(0)(_ + _))
  }

  override def onComplete: Unit = {
    println("[fold] onComplete")
    _sub.onNext(data.foldRight(0)(_ + _))
    data.clear()
    _sub.onComplete
  }
}


object RxProto {
  def main(args:Array[String]): Unit = {
    val air = PubAir()
    val sensor = SubSensor()
    val opFold = FoldOperator()
    val opMap = MapOperator()

    //  todo: unnatural chaining
    opMap.init(air, opFold)
    opFold.init(opMap, sensor)

    air.subscribe(opMap)
  }
}