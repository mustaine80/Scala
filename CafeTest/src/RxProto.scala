/*
  reactive stream simply implementation
  protocol: onSubscribe onNext* (onError | onComplete)
 */


sealed abstract class Publisher {
  //  todo: when I request to subscribe, inform my demand
  def subscribe(sub: Subscriber): Unit
}


sealed abstract class Subscriber {
  def onSubscribe(s: Subscription): Unit  //  todo: ????

  def onNext(t: Int): Unit    //  todo: generics

  def onError(t: Throwable): Unit

  def onComplete: Unit
}


sealed abstract class Subscription {
  def cancel: Unit

  def request(n: Int): Unit   //  todo: argument  Int => QoS ?
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


//  todo: need to Processor Impl
case class MapOperator(sub: Subscriber) extends Subscriber {
  override def onSubscribe(s: Subscription): Unit = {
    println("[map] onSubscribe")
    sub.onSubscribe(s)
  }

  override def onNext(t: Int): Unit = {
    println(s"[map] onNext " + t * 2)
    sub.onNext(t * 2)
  }

  override def onError(t: Throwable): Unit = println("[map] onError: " + t.printStackTrace())

  override def onComplete: Unit = {
    println("[map] onComplete")
    sub.onComplete
  }
}


//  todo: need to Processor Impl
case class FoldOperator(sub: Subscriber) extends Subscriber {
  import scala.collection.mutable.ListBuffer

  val data = ListBuffer.empty[Int]

  override def onSubscribe(s: Subscription): Unit = {
    println("[fold] onSubscribe")
    sub.onSubscribe(s)
  }

  override def onNext(n: Int): Unit = {
    data += n
    println(s"[fold] onNext - " + data.foldRight(0)(_ + _))
  }

  override def onError(t: Throwable): Unit = println("[fold] onError: " + t.printStackTrace())

  override def onComplete: Unit = {
    println("[fold] onComplete")
    sub.onNext(data.foldRight(0)(_ + _))
    data.clear()
    sub.onComplete
  }
}


object RxProto {
  def main(args:Array[String]): Unit = {
    val air = PubAir()
    val sensor = SubSensor()
    val opFold = FoldOperator(sensor)
    val opMap = MapOperator(FoldOperator(sensor))

    air.subscribe(opMap)
  }
}