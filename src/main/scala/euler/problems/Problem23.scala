package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem23 extends App {

  def isAbundant(n: Int): Boolean = AdvancedMath.properDivisors(n).sum > n

  lazy val abundantNumbers = Stream.from(12)
    .takeWhile(_ < 28112)
    .filter(isAbundant)
    .toList

  lazy val abundantNumbersSums = (for {
    a <- abundantNumbers
    b <- abundantNumbers.takeWhile(_ <= a)
    if a + b <= 28123
  } yield {
    a + b
  }).distinct.sorted

  lazy val notSummables = (1 until abundantNumbersSums.head) ++ abundantNumbersSums.zip(abundantNumbersSums.tail).flatMap((a: (Int, Int)) => {
    (a._1 + 1) until a._2
  }) ++ ((abundantNumbersSums.last + 1) until 28124 )

  lazy val sum = notSummables.sum

  println(sum)
}
