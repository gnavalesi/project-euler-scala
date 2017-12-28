package euler.problems

import euler.utils.Problem

/**
  * Problem 56
  *
  * A googol (10&#94;100) is a massive number: one followed by one-hundred zeros; 100&#94;100 is almost unimaginably large:
  * one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
  *
  * Considering natural numbers of the form, a&#94;b, where a, b < 100, what is the maximum digital sum?
  *
  **/
object Problem56 extends Problem with App {

  def powersStream(a: BigInt): Stream[BigInt] = powersStream(a, 100, 1)

  def powersStream(a: BigInt, b: Int, acc: BigInt): Stream[BigInt] = if(b == 0) Stream.empty else {
    val newAcc = acc * a
    newAcc #:: powersStream(a, b - 1, newAcc)
  }

  override def solution(): Any = (BigInt(1) until BigInt(100))
    .flatMap(powersStream)
    .map(_.toString())
    .map(_.split(""))
    .map(_.map(_.toInt))
    .map(_.sum)
    .max
}
