package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem43 extends Problem {
  private def pandigitalNumbers(): Stream[Seq[Int]] = (0 until 10).permutations.toStream
    .filter(p => p(0) > 0)

  private def isSubdivisible(n: Seq[Int]): Boolean = {
    n.tail.zip(n.tail.tail)
      .map(a => a._1.toString + a._2.toString)
      .zip(n.tail.tail.tail)
      .map(a => a._1 + a._2.toString)
      .map(_.toInt)
      .zip(Primes.int.stream)
      .forall(a => a._1 % a._2 == 0)
  }

  override def solution(): Any = pandigitalNumbers()
    .filter(isSubdivisible)
    .map(n => n.mkString.toLong)
    .sum
}
