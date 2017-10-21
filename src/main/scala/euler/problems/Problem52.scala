package euler.problems

import euler.utils.Problem

/**
  * Problem 52
  *
  * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
  * but in a different order.
  *
  * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
  **/
object Problem52 extends Problem with App {

  private def multiples(n: Int): Seq[Int] = (1 until 7).map(n * _)

  override def solution(): Any = Stream.from(1)
    .find(n => multiples(n)
      .map(_.toString.sorted)
      .forall(m => m.equals(n.toString.sorted))).get
}
