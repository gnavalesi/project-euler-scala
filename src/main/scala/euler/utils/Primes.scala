package euler.utils

/**
  * @author guido
  */

object Primes {

  object long {
    private def primesStreamRec(from: Long): Stream[Long] =
      if (isPrime(from)) from #:: primesStreamRec(from + 2) else primesStreamRec(from + 2)

    private def primesStream: Stream[Long] = 2 #:: primesStreamRec(3L)

    def isPrime(n: Long): Boolean = !stream.takeWhile(p => p * p <= n).exists(p => n % p == 0)

    val stream: Stream[Long] = primesStream
  }

  object bigInt {
    private def primesStreamRec(from: BigInt): Stream[BigInt] =
      if (isPrime(from)) from #:: primesStreamRec(from + 2) else primesStreamRec(from + 2)

    private def primesStream: Stream[BigInt] = 2 #:: primesStreamRec(scala.math.BigInt(3))

    def isPrime(n: BigInt): Boolean = !stream.takeWhile(p => p * p <= n).exists(p => n % p == 0)

    val stream: Stream[BigInt] = primesStream
  }

  object int {
    private def primesStreamRec(from: Int): Stream[Int] =
      if (isPrime(from)) from #:: primesStreamRec(from + 2) else primesStreamRec(from + 2)

    private def primesStream: Stream[Int] = 2 #:: primesStreamRec(3)

    def isPrime(n: Int): Boolean = !stream.takeWhile(p => p * p <= n).exists(p => n % p == 0)

    val stream: Stream[Int] = primesStream
  }

}

