package euler.problems

import euler.utils.{Problem, Primes}

/**
  * Problem 46
  *
  * It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and
  * twice a square.
  *
  * 9 = 7 + 2×1^2
  * 15 = 7 + 2×2^2
  * 21 = 3 + 2×3^2
  * 25 = 7 + 2×3^2
  * 27 = 19 + 2×2^2
  * 33 = 31 + 2×1^2
  *
  * It turns out that the conjecture was false.
  *
  * What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
  *
  */
object Problem46 extends Problem with App {

  private def isOdd(n: Int): Boolean = n % 2 == 1

  private def isEven(n: Int): Boolean = n % 2 == 0

  override def solution(): Any = Stream.from(2)
    .filter(isOdd)
    .filterNot(Primes.int.isPrime)
    .filterNot(n => Primes.int.stream
      .takeWhile(p => p < n)
      .map(p => n - p)
      .filter(isEven)
      .map(_ / 2)
      .exists(Math.sqrt(_) % 1 == 0))
    .head
}
