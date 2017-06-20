package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem21 extends Problem {


  private val sum = (1L until 10000L)
    .map((n: Long) => (n, AdvancedMath.properDivisors(n).sum))
    .filter(a => a._1 != a._2)
    .filter(a => AdvancedMath.properDivisors(a._2).sum == a._1)
    .map(_._1)
    .distinct

  override def solution(): Any = sum.sum
}
