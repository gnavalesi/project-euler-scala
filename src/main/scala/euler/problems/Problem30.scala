package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem30 extends Problem {
  def sumOfPowersOfDigits(n: Int): Int = n.toString.split("").map(c => Math.pow(c.toInt.toDouble, 5).toInt).sum

  private lazy val maxDigit = Math.pow(9, 5).toInt

  def canBeWrittenAsSum(n: Int): Boolean =
    if(n.toString.length * maxDigit < n) false
    else sumOfPowersOfDigits(n) == n

  private lazy val nums = for {
    i <- Stream.from(2).takeWhile(n => n.toString.length * maxDigit >= n) if canBeWrittenAsSum(i)
  } yield i

  override def solution(): Any = nums.sum
}
