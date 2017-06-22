package euler.problems

import euler.utils.{Primes, Problem}

/**
  * Problem 37
  *
  * The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from
  * left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
  * 379, 37, and 3.
  *
  * Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
  *
  * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  *
  */
object Problem37 extends Problem with App {

  private def primesThatStartWith(n: Long): Stream[Long] = Primes.long.stream(n * 10)
    .takeWhile(p => p < (n + 1) * 10)
    .filter(p => p - n * 10 < 10 && p - n * 10 >= 0)

  private def isLeftTruncatable(n: Long): Boolean = Stream.from(1)
    .takeWhile(i => i < n.toString.length)
    .map(i => n.toString.substring(i, n.toString.length).toLong)
    .forall(i => Primes.long.isPrime(i))

  def rightTruncatablePrimes(): Stream[Long] = rightTruncatablePrimes(Primes.long.stream.takeWhile(_ < 10), Stream.empty[Long])

  def rightTruncatablePrimes(primes: Stream[Long], acc: Stream[Long]): Stream[Long] =
    primes.flatMap(primesThatStartWith) match {
      case x #:: xs => rightTruncatablePrimes(x #:: xs, x #:: xs ++ acc)
      case _ => acc
    }

  def solution(): Long = rightTruncatablePrimes().filter(isLeftTruncatable).sum

  println(solution())
}
