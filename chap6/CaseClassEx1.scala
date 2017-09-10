//	패턴 종류에 따른 패턴 매치 예제

abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object CaseClassEx extends App {
	val expr = BinOp("-", Number(1.0), Var("x"))
	expr match {
		case BinOp(_, _, _) => println(expr + " is a binaray operation.")	///	와일드카드 패턴
		case _ => println("It's something else.")
	}

	def describe(x: Any): String = x match {	///	상수 패턴
		case 5 => "five"
		case true => "truth"
		case "hello" => "hi"
		case Nil => "the empty list"
		case _ => "something else"
	}

	for (seq <- Seq(5, true, "hello", Nil)) {
		println(describe(seq))
	} 

	val bar = 1
	bar match {
		case 0 => println("zero")
		case somethingElse => println("not zero: " + somethingElse)	///	변수 패턴
	}

	import math.{E, Pi}
	val pi = math.Pi
	///	소문자 이름은 컴파일러에서 패턴 변수로 인식하기 때문에 상수로 사용하려면
	///	this.pi 나 obj.pi 같이 사용하거나 ``을 붙여 사용해야 한다.
	E match {
		//case Pi => println("strange math? Pi = " + Pi)
		case `pi` => println("strange math? Pi = " + pi)	
		case _ => println("OK")
	}

	val expr1 = BinOp("+", Var("e"), Number(0))
	expr1 match {
		case BinOp("+", Var("e"), Number(0)) => println("a deep match")	///	생성자 패턴
		case _ => println("something else")
	}

	List(0,1,2) match {
		case List(0, _, _) => println("found it")	///	길이가 정해진 시퀀스 패턴
		case _ => println("something else")
	}

	List(0,1,2,3,4,5,6,7) match {
		case List(0, _*) => println("found it")		///	길이와 관계없이 매칭하는 시퀀스 패턴
		case _ => println("something else")
	}

	def tupleDemo(expr: Any) = expr match {
		case (a,b,c) => println("matched " + a + b + c)	///	튜플 패턴
		case _ => println("something else")
	}

	tupleDemo(("a ", 3, " -tuple"))

	def generalSize(x: Any): Int = x match {
		case s: String => s.length	///	타입 지정 패턴
		case m: Map[_, _] => m.size
		case _ => -1
	}

	println("String size : " + generalSize("Hello, world!"))
	println("Map size : " + generalSize(Map(1->'a', 2->'b')))

	val expr2 = UnOp("abs", UnOp("abs", Var("e")))
	expr2 match {
		case UnOp("abs", e @ UnOp("abs", _)) => println(e)	///	변수 바인딩
		case _ =>
	}

	def simplifyAdd(e: Expr) = e match {
		case BinOp("+", x, y)  if x == y => BinOp("*", x, Number(2))	///	패턴 가드
		case _ => e
	}

	println(simplifyAdd(BinOp("+", Var("x"), Var("x"))))
}