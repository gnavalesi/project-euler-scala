package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem20 extends App {
  val sum = AdvancedMath.factorial(100).toString().split("").map(_.toInt).sum
  println(sum)
}
