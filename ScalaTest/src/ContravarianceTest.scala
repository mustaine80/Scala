abstract class Event {
  def event = println("Event")
}

class PressEvent extends Event {
  override def event = println("PressEvent")
}

class UIObject

class Button extends UIObject {
  type EventHandler = (Button, PressEvent) => Unit
  var callback : EventHandler = handler
  
  def click() {
    callback(this, new PressEvent)
  }
  
  def handler(o: Button, e: PressEvent) {
    println("Button's handler!")
  }
}

object ContravarianceTest extends App {
  def handler(o: UIObject, e: Event): Unit = {
    println("UI's handler!")
    e.event
  }
  
  def handler2(b: Button, e: PressEvent): Unit = {
    println("handler2!!!")
    e.event
  }
  
  val b = new Button
  b.callback = handler
  
  b.click
  
  b.callback = handler2
  
  b.click
}