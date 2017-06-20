package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem20 extends Problem {
  override def solution(): Any = AdvancedMath.factorial(100).toString().split("").map(_.toInt).sum
}
