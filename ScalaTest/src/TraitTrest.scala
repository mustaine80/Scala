trait Logged {
  def log(msg: String)
  def info(msg: String) { log("INFO: " + msg) }
  def warn(msg: String) { log("WARN: " + msg) }
  def severe(msg: String) { log("SEVERE: " + msg) }
}

class MyAccount {
  var balance = 0
}

abstract class SavingAccount extends MyAccount with Logged {
  def withdraw(amount: Double) {
    if(amount > balance) severe("Insufficient funds")
  }
}

trait ConsoleLogger extends Logged {
  override def log(msg: String) { println(msg) }
}

trait TimeStampLogger extends Logged {
  abstract override def log(msg: String) {
    super.log(new java.util.Date() + " " + msg)
  }
}

trait ShortLogger extends Logged {
  val maxLength = 15
  abstract override def log(msg: String) {
    super.log(
        if(msg.length <= maxLength) msg else msg.substring(0, maxLength) + "...")
  }
}

trait LoggedException extends Exception with Logged {
  def log() { log(getMessage()) }
}

abstract class UnhappyException extends LoggedException {
  override def getMessage() = "arggh!"
}

object TraitTest2 extends App {
  //var acc = new SavingAccount    // acc can't be instantiated because SavingAccount it abstract class
  var acc2 = new SavingAccount with ConsoleLogger
  var acc3 = new SavingAccount with ConsoleLogger with TimeStampLogger
  var acc4 = new SavingAccount with ConsoleLogger with ShortLogger with TimeStampLogger
  var acc5 = new SavingAccount with ConsoleLogger with TimeStampLogger with ShortLogger
 
  //acc.withdraw(1100)
  acc2.withdraw(1100)
  acc3.withdraw(1100)
  acc4.withdraw(1100)
  acc5.withdraw(1100)
  
  acc2.info("this is test msg")
  acc3.info("this is test msg")
  acc4.info("this is test msg")
  acc5.info("this is test msg")
}