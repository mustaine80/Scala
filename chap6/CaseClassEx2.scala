//  한주영 씨 블로글에서 발췌한 소스

//  OOP solution
trait Expr {
  def eval: Int
  def print: String
}

class Number(value: Int) extends Expr {
  override def eval: Int = value
  override def print: String = value.toString
}

class Sum(left: Expr, right: Expr) extends Expr {
  override def eval: Int = left.eval + right.eval
  override def print: String = left.print + "+" + right.print
}

class Product(left: Expr, right: Expr) extends Expr {
  override def eval: Int = left.eval * right.eval
  override def print: String = left.print + "*" + right.print
}

//  FP solution
trait ExprFP {
  def eval: Int = this match {
    case NumberFP(value) => value
    case SumFP(left, right) => left.eval + right.eval
    case ProductFP(left, right) => left.eval * right.eval
  }

  def print: String = this match {
    case NumberFP(value) => value.toString
    case SumFP(left, right) => left.print + "+" + right.print
    case ProductFP(left, right) => left.print + "*" + right.print
  }
}

case class NumberFP(value: Int) extends ExprFP {}
case class SumFP(left: ExprFP, right: ExprFP) extends ExprFP {}
case class ProductFP(left: ExprFP, right: ExprFP) extends ExprFP {}

object CaseClassEx extends App {
  println(new Sum(new Number(2), new Number(3)).print)
  println(new Product(new Number(2), new Number(3)).print)

  println(SumFP(NumberFP(2), NumberFP(3)).print)
  println(ProductFP(NumberFP(2), NumberFP(3)).print)
}
