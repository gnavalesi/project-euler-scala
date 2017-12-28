package euler.problems

import euler.utils.Problem

/**
  * Problem 58
  *
  * Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
  *   (37)  36   35   34   33   32  (31)
  *    38  (17)  16   15   14  (13)  30
  *    39   18  ( 5)   4  ( 3)  12   29
  *    40   19    6    1    2   11   28
  *    41   20  ( 7)   8    9   10   27
  *    42   21   22   23   24   25   26
  *   (43)  44   45   46   47   48   49
  *
  * It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is
  * that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
  *
  * If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
  * If this process is continued, what is the side length of the square spiral for which the ratio of primes along both
  * diagonals first falls below 10%?
  * *
  **/
object Problem58 extends Problem with App {

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
