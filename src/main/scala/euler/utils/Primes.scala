package euler.utils

/**
  * @author guido
  */

object Primes {

  object long {
    private def isNotDivisibleBy(n: Long, divisors: Set[Long]) = !divisors.exists(p => n % p == 0)

    private def primesStream(from: Long, previousPrimes: Set[Long]): Stream[Long] =
      if (isNotDivisibleBy(from, previousPrimes))
        from #:: primesStream(from + 2, previousPrimes + from)
      else primesStream(from + 2, previousPrimes)

    private def primesStream: Stream[Long] = 2 #:: primesStream(3L, Set(2L))

    def isPrime(n: Long): Boolean = stream.dropWhile(_ < n).head == n

    val stream: Stream[Long] = primesStream
  }

  object bigInt {
    private def isNotDivisibleBy(n: BigInt, divisors: Set[BigInt]) = !divisors.exists(p => n % p == 0)

    private def primesStream(from: BigInt, previousPrimes: Set[BigInt]): Stream[BigInt] =
      if (isNotDivisibleBy(from, previousPrimes)) from #:: primesStream(from + 2, previousPrimes + from)
      else primesStream(from + 2, previousPrimes)

    private def primesStream: Stream[BigInt] = 2 #:: primesStream(scala.math.BigInt(3), Set(scala.math.BigInt(2)))

    def isPrime(n: BigInt): Boolean = stream.dropWhile(_ < n).head == n

    val stream: Stream[BigInt] = primesStream
  }

  object int {
    private def isNotDivisibleBy(n: Int, divisors: Set[Int]) = !divisors.exists(p => n % p == 0)

    private def primesStream(from: Int, previousPrimes: Set[Int]): Stream[Int] =
      if (isNotDivisibleBy(from, previousPrimes))
        from #:: primesStream(from + 2, previousPrimes + from)
      else primesStream(from + 2, previousPrimes)

    private def primesStream: Stream[Int] = 2 #:: primesStream(3, Set(2))

    def isPrime(n: Int): Boolean = stream.dropWhile(_ < n).head == n

    val stream: Stream[Int] = primesStream
  }
}

