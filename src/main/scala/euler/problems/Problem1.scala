package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem1 extends Problem {

  private val from = 1
  private val to = 1000

  private def isDivisible(i: Int): Boolean = i % 3 == 0 || i % 5 == 0

  def solution(): Int = (from until to by 1).filter(isDivisible).sum
}
