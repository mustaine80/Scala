abstract class Tree
case class Sum(l: Tree, r: Tree) extends Tree
case class Var(n: String) extends Tree
case class Const(v: Int) extends Tree

object PatternMatchTest
{
  type Environment = String => Int
  
  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n) => env(n)
    case Const(v) => v
  }
  
  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    case Var(n) if(v == n) => Const(1)
    case _ => Const(0)
  }
  
  def main(args:Array[String]) {
    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Environment = { case "x" => 5 case "y" => 7 }
    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: " + eval(exp, env))
    println("Derivative relative to x:\n " + derive(exp, "x"))
    println("Derivative relative to y:\n " + derive(exp, "y"))
  }
}

object Main extends App {
  object Twice {
    def apply(x: Int) : Int = x * 2
    def unapply(x: Int) : Option[Int] = if(x % 2 == 0) Some(x / 2) else None
  }
  
  val x = Twice(21)
  x match {
    case Twice(n) => println(x + " is twice of " + n)
  }
  
  object Test {
    def unapply(a: Int) : Boolean = {
      if(a < 10) true else false
    }
  }
  
  object Test2 {
    def unapply(a: Int): Option[(Int, String)] = {
      if(a > 30) Some(a/10, "from test2") else None
    }
  }
  
  class Example {
    def method(target: Int) {
      target match {
        case Test() => println("matched to Test")
        case Test2(n @ Test(), m) => println("result : " + n + " " + m)
        case 120 => println("matched to 120")
        case _ => println("not matched")
      }
    }
  }
  
  val t = new Example
  t.method(5)
  t.method(40)
  t.method(120)
  
  object Domain {
    def unapplySeq(whole: String): Option[Seq[String]] = {
      //Some(whole.split("\\.").reverse)
      Some(whole.split("\\."))
    }
  }
  
  "www.naver.com" match {
    case Domain(first, second, third) => printf(" %s %s %s !!\n", first, second, third)
    case _ => "?"
  }
  
  val Domain(first, second, third) = "www.lignex1.com"
  
  printf("%s %s %s\n", first, second, third)
}
