package euler.problems

import euler.utils.Problem

/**
  * Problem 38
  *
  * Take the number 192 and multiply it by each of 1, 2, and 3:
  * 192 × 1 = 192
  * 192 × 2 = 384
  * 192 × 3 = 576
  *
  * By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated
  * product of 192 and (1,2,3)
  *
  * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645,
  * which is the concatenated product of 9 and (1,2,3,4,5).
  *
  * What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer
  * with (1,2, ... , n) where n > 1?
  *
  */
object Problem38 extends Problem {
  private def isPandigital(n: Long): Boolean = n.toString.replace("0", "").distinct.length == 9

  private def concatMultiples(multiples: Stream[String]): Long = concatMultiples(multiples, "")

  private def concatMultiples(multiples: Stream[String], acc: String): Long =
    if (multiples.head.length + acc.length > 9) acc.toLong
    else concatMultiples(multiples.tail, acc + multiples.head)

  override def solution(): Any =
    Stream
      .from(1)
      .takeWhile(n => n <= 10000)
      .map(n =>
        concatMultiples(Stream
          .from(1)
          .map(i => i * n)
          .map(i => i.toString)
        )
      )
    .filter(isPandigital)
    .max
}
