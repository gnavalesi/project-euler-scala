package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem2 extends Problem {

  def solution(): BigInt = AdvancedMath.fibonacci.filter(_ % 2 == 0).takeWhile(n => n < 4000000).sum
}
