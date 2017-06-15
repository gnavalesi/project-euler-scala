package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem10 extends App {
  val sum = AdvancedMath.primesUntil(2000000).sum

  println(sum)
}
