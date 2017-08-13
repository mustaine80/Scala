class Account(val id: Int, initialBalance: Double) {
  private var balance = initialBalance
  def deposit(amount: Double) {
    balance += amount
  }
}

object Account {
  private var lastNumber = 0
  private def newUniqueNumber() = {
    lastNumber += 1
    lastNumber
  }
  
  def apply(initialBalance: Double) = new Account(newUniqueNumber(), initialBalance)
}

abstract class UndoableAction(val description: String) {
  def undo(): Unit
  def redo(): Unit
}

object DoNothingAction extends UndoableAction("Do Nothing") {
  override def undo() {}
  override def redo() {}
}

object ObjectTest extends App {
  val actions = Map("open" -> DoNothingAction, "save" -> DoNothingAction)
  println(actions)
  
  val acct = Account(1000.0)
  println("account id : " + acct.id)
}