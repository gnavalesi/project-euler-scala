package euler.problems

import euler.utils.Problem

/**
  * Problem 57
  *
  * It is possible to show that the square root of two can be expressed as an infinite continued fraction.
  *
  * √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
  *
  * By expanding this for the first four iterations, we get:
  * 1 + 1/2 = 3/2 = 1.5
  * 1 + 1/(2 + 1/2) = 7/5 = 1.4
  * 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
  * 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
  *
  * The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example
  * where the number of digits in the numerator exceeds the number of digits in the denominator.
  *
  * In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
  *
  * ====================================================================================================================
  *
  * Solution:
  *
  * Lets start by getting the expansions fractions without the +1
  * 1/2
  * 1/(2 + 1/2) = 2/5
  * 1/(2 + 1/(2 + 1/2)) = 5/12
  * 1/(2 + 1/(2 + 1/(2 + 1/2))) = 12/29
  * 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/2)))) = 29/70
  * 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/2))))) = 70/169
  * 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/(2 + 1/2)))))) = 169/408
  * ...
  *
  * As we see, in each iteration the denominator becomes the numerator in the next iteration. Ok, now we have a pattern
  * for the numerator. But what about the denominator?
  *
  * Lets start by subtracting denominators to find a pattern:
  * 12 - 5 = 7 = 2 + 5 => 12 = 2 + 2*5
  * 29 - 12 = 17 = 5 + 12 => 29 = 5 + 2*12
  * 70 - 29 = 41 = 12 + 29 => 70 = 12 + 2*29
  *
  * The denominator is the sum of the numerator and two times the denominator of the previous iteration.
  *
  * Now we have a pattern to create a stream of expansions and count how many contain a numerator with more digits than
  * denominator
  *
  **/
object Problem57 extends Problem with App {

  def expansionsStream(): Stream[(BigInt, BigInt)] = (BigInt(3), BigInt(2)) #:: expansionsStream(1, 2)

  def expansionsStream(previousNumerator: BigInt, previousDenominator: BigInt): Stream[(BigInt, BigInt)] = {
    val numerator = previousDenominator
    val denominator = previousNumerator + 2 * previousDenominator

    (denominator + numerator, denominator) #:: expansionsStream(numerator, denominator)
  }

  def toDigitsSize(a: BigInt): BigInt = a.toString.length

  def toDigitsSize(pair: (BigInt, BigInt)): (BigInt, BigInt) = (toDigitsSize(pair._1), toDigitsSize(pair._2))

  override def solution(): Any = expansionsStream()
    .take(1000)
    .map(toDigitsSize)
    .count(a => a._1 > a._2)

  println(solution())
}
