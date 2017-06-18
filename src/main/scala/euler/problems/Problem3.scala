package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem3 extends Problem {
  val n: Long = 600851475143L

  def solution(): BigInt = AdvancedMath.primeFactors(n).maxBy(_._1)._1
}
