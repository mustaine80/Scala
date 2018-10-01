/*
  reactive stream simply implementation
  protocol: onSubscribe onNext* (onError | onComplete)
 */


sealed trait Publisher {
  def subscribe(sub: Subscriber): Unit
}


sealed trait Subscriber {
  def onSubscribe(s: Subscription): Unit

  def onNext(t: Int): Unit    //  todo: generics

  def onError(t: Throwable): Unit = println(t.printStackTrace())

  def onComplete: Unit
}


sealed abstract class Subscription {
  def cancel: Unit

  def request(n: Int): Unit   //  todo: argument  Int => QoS ?
}


sealed trait Processor extends Publisher with Subscriber {
  def _sub: Subscriber

  override def subscribe(sub: Subscriber): Unit = Unit

  override def onSubscribe(s: Subscription): Unit = _sub.onSubscribe(s)
}


case class PubAir(_sub: Subscriber) extends Publisher {
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

  subscribe(_sub)
}


case class SubSensor() extends Subscriber {
  override def onSubscribe(s: Subscription): Unit = {
    println("[Sensor] onSubscribe")
    s.request(10)
  }

  override def onNext(t: Int): Unit = println("[Sensor] onNext - " + t)

  override def onComplete: Unit = println("[Sensor] onComplete\n")
}


case class MapOperator(sub: Subscriber) extends Processor {
  override def _sub: Subscriber = sub

  override def onNext(t: Int): Unit = _sub.onNext(t * 2)

  override def onComplete: Unit = _sub.onComplete
}


case class FoldOperator(sub: Subscriber) extends Processor {
  import scala.collection.mutable.ListBuffer
  val data = ListBuffer.empty[Int]

  override def _sub: Subscriber = sub

  override def onNext(t: Int): Unit = { data += t }

  override def onComplete: Unit = {
    _sub.onNext(data.foldRight(0)(_ + _))
    data.clear()
    _sub.onComplete
  }
}


object RxProto {
  def main(args:Array[String]): Unit = {
    val air_map_fold_sensor = PubAir(MapOperator(FoldOperator(SubSensor())))
  }
}