package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem16 extends Problem {
  private val pow = BigInt(2).pow(1000)

  def solution(): Int = pow.toString.split("").map(_.toInt).sum
}
