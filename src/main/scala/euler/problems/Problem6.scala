package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem6 extends Problem {
  private case class Accumulator(sumOfPowers: Long, sum: Long)

  private val sums = (1L until 101L).foldLeft(Accumulator(0L, 0L))((acc: Accumulator, n: Long) => {
    Accumulator(acc.sumOfPowers + Math.pow(n, 2).toLong, acc.sum + n)
  })

  def solution(): Long = Math.pow(sums.sum, 2).toLong - sums.sumOfPowers
}
