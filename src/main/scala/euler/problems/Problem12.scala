package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem12 extends App {
  def triangleNumbers(n: Long, acc: Long): Stream[Long] = (n + acc) #:: triangleNumbers(n + 1, n + acc)

  val triangleStream = triangleNumbers(1, 0)
  val first = triangleStream.map(t => {
    val factors = AdvancedMath.factors(t)
    println(s"triangle $t factors ${factors.length}")
    factors
  }).filterNot(_.length < 500).head

  println(first)
}
