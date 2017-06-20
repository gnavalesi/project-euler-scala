package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem10 extends Problem {
  private val sum = Primes.long.stream.takeWhile(_ <= 2000000).sum

  override def solution(): Long = sum
}
