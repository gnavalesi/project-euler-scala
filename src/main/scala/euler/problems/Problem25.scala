package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem25 extends Problem {
  lazy private val num: BigInt = AdvancedMath.fibonacci.indexWhere(a => a.toString().length == 1000) + 1

  override def solution(): Any = num
}
