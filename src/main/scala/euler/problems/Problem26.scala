package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem26 extends Problem {

  case class Result(result: Int, remainder: Int)

  def divideUnitBy(n: Int): Result = {
    Result(10 / n, 10 % n)
  }

  def divideBy(n: Int, d: Int): Result = Result(n * 10 / d, (n * 10) % d)

  def getRecurringCycle(n: Int): Seq[Int] = {
    getRecurringCycle(1, n, Seq.empty[Int], Seq.empty[Int])
  }

  def getRecurringCycle(n: Int, d: Int, acc: Seq[Int], previousN: Seq[Int]): Seq[Int] = {
    divideBy(n, d) match {
      case Result(result: Int, remainder: Int) =>
        if(remainder == 0) Seq.empty[Int]
        else if(remainder == n) Seq(result)
        else if(previousN.contains(remainder)) acc.slice(previousN.indexOf(remainder), acc.length) :+ result
        else getRecurringCycle(remainder, d, acc :+ result, previousN :+ n)
    }
  }

  lazy private val cycles: Seq[(Int, Int)] = for {
    n <- 2 until 1000 if 1000000000 % n != 0
    cx = getRecurringCycle(n) if cx.nonEmpty
  } yield (n, cx.length)

  lazy private val max: (Int, Int) = cycles.maxBy(_._2)

  override def solution(): Any = max._1
}
