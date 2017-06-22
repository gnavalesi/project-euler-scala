package euler.utils

/**
  * @author guido
  */

object Primes {

  object long {
    private def primesStreamRec(from: Long): Stream[Long] =
      if (isPrime(from)) from #:: primesStreamRec(from + (if(from % 2 == 0) 1 else 2)) else primesStreamRec(from + (if(from % 2 == 0) 1 else 2))

    private def primesStream: Stream[Long] = 2 #:: primesStreamRec(3L)

    def isPrime(n: Long): Boolean = n != 1 && stream.takeWhile(p => p * p <= n).forall(p => n % p != 0)

    val stream: Stream[Long] = primesStream

    def stream(from: Long): Stream[Long] = primesStreamRec(from)
  }

  object bigInt {
    private def primesStreamRec(from: BigInt): Stream[BigInt] =
      if (isPrime(from)) from #:: primesStreamRec(from + (if(from % 2 == 0) 1 else 2)) else primesStreamRec(from + (if(from % 2 == 0) 1 else 2))

    private def primesStream: Stream[BigInt] = 2 #:: primesStreamRec(scala.math.BigInt(3))

    def isPrime(n: BigInt): Boolean = n != 1 && stream.takeWhile(p => p * p <= n).forall(p => n % p != 0)

    val stream: Stream[BigInt] = primesStream
  }

  object int {
    private def primesStreamRec(from: Int): Stream[Int] =
      if (isPrime(from)) from #:: primesStreamRec(from + (if(from % 2 == 0) 1 else 2)) else primesStreamRec(from + (if(from % 2 == 0) 1 else 2))

    private def primesStream: Stream[Int] = 2 #:: primesStreamRec(3)

    def isPrime(n: Int): Boolean = n != 1 && stream.takeWhile(p => p * p <= n).forall(p => n % p != 0)

    val stream: Stream[Int] = primesStream
  }

}

