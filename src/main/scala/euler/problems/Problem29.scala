package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem29 extends Problem {
  private lazy val values = (for {
    a <- 2 until 101
    b <- 2 until 101
  } yield BigInt(a).pow(b)).distinct.length

  override def solution(): Any = values
}
