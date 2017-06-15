package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem23 extends App {

  def isAbundant(n: Int): Boolean = AdvancedMath.properDivisors(n.toLong).sum > n

  lazy val abundantNumbers = Stream.from(12)
    .takeWhile(_ < 28123)
    .filter(isAbundant)

  def canBeWrittenAsSum(n: Int): Boolean =
    abundantNumbers.takeWhile(s => s <= n - abundantNumbers.head)
      .exists(s => abundantNumbers.contains(n - s))

  def notSumablesFrom(n: Int, acc: Seq[Int]): Seq[Int] = {
    println(n)
    if(n < 28123)
      if(canBeWrittenAsSum(n)) notSumablesFrom(n + 1, acc)
      else notSumablesFrom(n + 1, acc :+ n)
    else acc
  }

  val sum = notSumablesFrom(1, Seq()).sum

  println(sum)
}
