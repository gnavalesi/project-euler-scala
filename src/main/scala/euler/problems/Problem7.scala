package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem7 extends Problem {
  def solution(): Int = Primes.int.stream.drop(10000).head
}
