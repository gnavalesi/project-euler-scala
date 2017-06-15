package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem21 extends App {

  println(AdvancedMath.properDivisors(220))
  println(AdvancedMath.properDivisors(220).sum)
  println(AdvancedMath.properDivisors(AdvancedMath.properDivisors(220).sum))
  println(AdvancedMath.properDivisors(AdvancedMath.properDivisors(220).sum).sum)


  val sum = (1L until 10000L)
    .map((n: Long) => (n, AdvancedMath.properDivisors(n).sum))
    .filter(a => a._1 != a._2)
    .filter(a => AdvancedMath.properDivisors(a._2).sum == a._1)
    .map(_._1)
    .distinct

  println(sum)
  println(sum.sum)


}
