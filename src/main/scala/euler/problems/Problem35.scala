package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem35 extends Problem with App {

  private def hasEvenDigit(n: Int): Boolean = n.toString.matches(".*[02468].*")

  private def isCircular(n: Int): Boolean = {
    (n == 2 || !hasEvenDigit(n)) && isCircular(n, n.toString.length - 1)
  }

  private def isCircular(n: Int, count: Int): Boolean = {
    val str = n.toString
    val r = (str.charAt(str.length - 1) + str.substring(0, str.length - 1)).toInt

    count == 0 || (Primes.int.isPrime(r) && isCircular(r, count - 1))
  }

  def solution(): Int = Primes.int.stream.takeWhile(_ < 1000000).count(isCircular)
}
