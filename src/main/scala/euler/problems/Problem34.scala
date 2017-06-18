package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem34 extends App {
  def digits(n: Int): Seq[Int] = n.toString.split("").map(_.toInt)

  def isSumOfDigitFactorials(n: Int): Boolean = {
    println(n)
    val ds = digits(n)
    n == ds.map(d => AdvancedMath.factorial(BigInt(d))).sum
  }

  val max = BigInt(362880) // 9!

  val numbers = Stream.from(10).takeWhile(n => n <= (Math.log10(n).ceil.toInt + 1) * max).filter(isSumOfDigitFactorials)

  println(numbers)

  println(numbers.sum)
}
