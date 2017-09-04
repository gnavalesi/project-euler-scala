package euler.problems

import euler.utils.{Primes, Problem}

/**
  * Problem 50
  *
  * The prime 41, can be written as the sum of six consecutive primes:
  * 41 = 2 + 3 + 5 + 7 + 11 + 13
  * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
  *
  * The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
  *
  * Which prime, below one-million, can be written as the sum of the most consecutive primes?
  */
object Problem50 extends Problem with App {

  private case class PrimeSum(prime: Int, length: Int)

  private def largestPrimeSum(ps: Stream[Int]): PrimeSum = largestPrimeSum(ps, 0, 0, PrimeSum(0, 0))

  private def largestPrimeSum(primes: Stream[Int], acc: Int, length: Int, last: PrimeSum): PrimeSum = primes match {
    case Stream.Empty => last
    case p #:: _ if acc + p >= 1000000 => last
    case p #:: ps if Primes.int.isPrime(acc + p) => largestPrimeSum(ps, acc + p, length + 1,
      PrimeSum(acc + p, length + 1))
    case p #:: ps => largestPrimeSum(ps, acc + p, length + 1, last)
  }

  private def largestPrimeSums(primes: Stream[Int]): Stream[PrimeSum] = primes match {
    case Stream.Empty => Stream.Empty
    case ps => largestPrimeSum(ps) #:: largestPrimeSums(ps.tail)
  }

  override def solution(): Any =
    largestPrimeSums(Primes.int.stream
      .takeWhile(p => p < 1000000))
    .reduce((p1, p2) => if (p1.length > p2.length) p1 else p2)
    .prime

  println(solution())


}
