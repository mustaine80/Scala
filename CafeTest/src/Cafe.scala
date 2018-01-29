case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else	//	todo: 복수의 결재 수단을 사용할 수 있어야 한다.
      throw new Exception("Can't combine charges to different cards")
  }
}

case class CreditCard(name: String)

case class Coffee(price: Double)


object Cafe {
  /** 커피 주문을 받으면 커피와 요금청구를 반환한다.
    * 커피는 고객에게 전달되어야 하겠지.
    * 요금청구는 결재 방법(현금, 카드)에 따라 잔돈을 거슬러 주거나 카드사에 청구해야 할 것이다.
    * 만약 우리 회사라면 사원증도 결재 방법으로 추가되어야 한다!!!
    */
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee(4.99)

    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc)) /// side effect 가 있을 경우 문제가 된다.
    val (coffees, charges) = purchases.unzip
    println(s"$n 개의 커피값 지불이 ${cc.name} 카드로 완료되었습니다.")

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def buyCoffeesWithCurring(cc: CreditCard)(n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    println(s"$n 개의 커피값 지불이 ${cc.name} 카드로 완료되었습니다.")

    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}


object CafeTest extends App {
  Cafe.buyCoffee(CreditCard("VISA"))
  //  Cafe.buyCoffees(CreditCard("America Express"), 10)

  /// 만약 우리 가게는 항상 VISA 카드만 받는다면 이것을 부분적용함수로 만들면 된다.
  val buyCoffeesWithVISA = Cafe.buyCoffees(CreditCard("VISA"), _: Int)
  buyCoffeesWithVISA(10)

  /// 결재부터 하는 상황에서는 커링이 효과적이다.
  val buyCoffeesPrepayment = Cafe.buyCoffeesWithCurring(CreditCard("법인")) _
  buyCoffeesPrepayment(20)
}
