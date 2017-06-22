package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem34 extends Problem {
  def digits(n: Int): Seq[Int] = n.toString.split("").map(_.toInt)

  def isSumOfDigitFactorials(n: Int): Boolean = n == digits(n).map(d => AdvancedMath.factorial(d)).sum

  private lazy val numbers = Stream.from(10).takeWhile(n => n <= 2540160L).filter(isSumOfDigitFactorials)

  override def solution(): Any = numbers.sum
}
