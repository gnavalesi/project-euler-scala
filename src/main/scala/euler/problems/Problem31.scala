package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem31 extends Problem {
  val total = 200

  private lazy val sum = (for {
    a <- 0 until total + 1
    b <- 0 until (total - a) / 2 + 1
    c <- 0 until (total - a - b * 2) / 5 + 1
    d <- 0 until (total - a - b * 2 - c * 5) / 10 + 1
    e <- 0 until (total - a - b * 2 - c * 5 - d * 10) / 20 + 1
    f <- 0 until (total - a - b * 2 - c * 5 - d * 10 - e * 20) / 50 + 1
    g <- 0 until (total - a - b * 2 - c * 5 - d * 10 - e * 20 - f * 50) / 100 + 1
    h <- 0 until (total - a - b * 2 - c * 5 - d * 10 - e * 20 - f * 50 - g * 100) / 200 + 1
    if a * 1 + b * 2 + c * 5 + d * 10 + e * 20 + f * 50 + g * 100 + h * 200 == total
  } yield 1).sum

  override def solution(): Any = sum
}
