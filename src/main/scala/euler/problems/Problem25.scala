package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem25 extends App {
  val num: BigInt = AdvancedMath.fibonacci.indexWhere(a => a.toString().length == 1000) + 1

  println(num)
}
