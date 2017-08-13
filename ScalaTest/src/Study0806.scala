object Study0806 extends App {
  val n = 10
  
  var n2 = if(n > 5) true else false
  
  println(n2)
  
  val arr = Array(10, 2, 3, 8, 4, 2, 9)
  
  val filterfunc = (t:Int) => t % 2 == 0
  val filterfunc2 = (t:Int) => t % 2 == 1
  
  for(i <- arr if i%2==0) println(i)
  arr.filter(filterfunc2).foreach(println)
  val filterArr = arr.filter(filterfunc).toArray
  
  println( filterArr.map(_.toString).reduce(_ + ", " + _) )
}