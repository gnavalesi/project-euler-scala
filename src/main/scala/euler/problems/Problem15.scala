package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem15 extends Problem {
  private val width = 20
  private val height = 20

  def solution(): Long = AdvancedMath.binomial(height + width, height)



  println(solution())
}
