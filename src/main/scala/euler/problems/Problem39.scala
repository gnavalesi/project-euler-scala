package euler.problems

import euler.utils.Problem

/**
  * Problem 39
  *
  * If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three
  * solutions for p = 120.
  *
  * {20,48,52}, {24,45,51}, {30,40,50}
  *
  * For which value of p ≤ 1000, is the number of solutions maximised?
  *
  */
object Problem39 extends Problem {

  override def solution(): Int =
    Stream
      .from(1)
      .takeWhile(p => p <= 1000)
      .map(p => (p,
        Stream
          .from(1)
          .takeWhile(a => a * 2 < p)
          .map(a => (a, (p * p - 2 * a * p).toDouble / (2 * p - 2 * a).toDouble))
          .count(r => r._2.ceil == r._2)
      ))
      .maxBy(r => r._2)
      ._1
}
