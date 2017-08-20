//	변경 가능한 상태를 갖지 않는 함수형 객체 예제
class Rational(n: Int, d: Int) {
	require(d != 0)
	private val g = gcd(n.abs, d.abs)
	val number = n / g
	val denon = d / g

	def this(n: Int) = this(n, 1)

	def + (that: Rational): Rational = {
		new Rational(
			number * that.denon + that.number * denon,
			denon * that.denon
		)
	}

	def + (i: Int): Rational = {
		new Rational(number + i * denon, denon)
	}

	def - (that: Rational): Rational = {
		new Rational(
			number * that.denon - that.number * denon,
			denon * that.denon
		)
	}

	def - (i: Int): Rational = {
		new Rational(number - i * denon, denon)
	}

	def * (that: Rational): Rational = {
		new Rational(number * that.number, denon * that.denon)
	}

	def * (i: Int): Rational = {
		new Rational(number * i, denon)
	}

	def / (that: Rational): Rational = {
		new Rational(number * that.denon, denon * that.number)
	}

	def / (i: Int): Rational = {
		new Rational(number, denon * i)
	}

	private def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}

	override def toString = number + "/" + denon
}

object MyApp {
	def main(args: Array[String]) {
		val a = new Rational(1,3)
		val b = new Rational(1,2)

		printf("\n1/3 + 1/2 = %s", a + b)
		printf("\n1/3 + 2 = %s", a + 2)
		printf("\n1/3 * 1/2 = %s", a * b)
		printf("\n1/3 * 2 = %s", a * 2)
		printf("\n1/3 - 1/2 = %s", a - b)
		printf("\n1/3 - 2 = %s", a - 2)
		printf("\n1/3 / 1/2 = %s", a / b)
		printf("\n1/3 / 2 = %s", a / 2)
	}
}
