package euler.problems

import euler.utils.Problem

/**
  * Problem 40
  *
  * An irrational decimal fraction is created by concatenating the positive integers:
  *
  * 0.123456789101112131415161718192021...
  *
  * It can be seen that the 12th digit of the fractional part is 1.
  *
  * If dn represents the nth digit of the fractional part, find the value of the following expression.
  *
  * d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000
  */
object Problem40 extends Problem with App {

  private val digitStream: Stream[Int] = digitStreamRec(1)

  private def digitStreamRec(n: Int): Stream[Int] = n.toString.split("").map(_.toInt).toStream #::: digitStreamRec(
    n + 1)

  override def solution(): Any = digitStream.head * digitStream(9) * digitStream(99) * digitStream(999) *
    digitStream(9999) * digitStream(99999) * digitStream(999999)


}
