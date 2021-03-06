package euler.problems

import euler.utils.Problem
import euler.utils.AdvancedMath.primeFactors

/**
  * @author guido
  */
object Problem5 extends Problem {

  def solution(): Int = (1 until 20)
    .flatMap(n => primeFactors(n))
    .groupBy(_._1)
    .mapValues(_.maxBy(_._2))
    .mapValues(_._2)
    .map(a => Math.pow(a._1, a._2.toInt))
    .product
    .toInt
}
