package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  *
  *
  */
object Problem12 extends Problem {

  private def factorizations(n: Long, nFactorization: Seq[(Long, Long)], triangle: Long): Stream[(Long, Long)] = {
    val np1Factorization = AdvancedMath.primeFactors(n + 1)
    (triangle + n, (nFactorization ++ np1Factorization).map(f => if(f._1 == 2) f._2 else f._2 + 1).product) #:: factorizations(n + 1, np1Factorization, triangle + n)
  }

  def solution(): Any = factorizations(1, Seq(), 0).dropWhile(_._2 < 500).head._1
}
