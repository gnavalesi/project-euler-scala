package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem9 extends Problem {
  private val value = for {
    c <- 3 until 1000
    b <- 2 until c
    a <- 1 until b if a + b + c == 1000 && Math.pow(a, 2) + Math.pow(b, 2) == Math.pow(c, 2)
  } yield a * b * c

  def solution(): Int = value.head
}
