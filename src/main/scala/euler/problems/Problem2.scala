package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem2 extends Problem {
  private val max = 4000000

  def solution(): BigInt = AdvancedMath.fibonacci.filter(_ % 2 == 0).takeWhile(n => n < max).sum
}
