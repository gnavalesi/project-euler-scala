package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem23 extends Problem {

  private def isAbundant(n: Int): Boolean = {
    val pd = AdvancedMath.properDivisors(n)
    println(n, pd)
    pd.sum > n
  }

  private lazy val abundantNumbers = Stream.from(12)
    .takeWhile(_ < 28112)
    .filter(isAbundant)
    .toList

  private lazy val abundantNumbersSums = (for {
    a <- abundantNumbers
    b <- abundantNumbers.takeWhile(_ <= a)
    if a + b <= 28123
  } yield {
    a + b
  }).distinct.sorted

  private lazy val notSummables = (1 until abundantNumbersSums.head) ++ abundantNumbersSums.zip(abundantNumbersSums.tail).flatMap((a: (Int, Int)) => {
    (a._1 + 1) until a._2
  }) ++ ((abundantNumbersSums.last + 1) until 28124 )

  def solution(): Int = notSummables.sum
}
