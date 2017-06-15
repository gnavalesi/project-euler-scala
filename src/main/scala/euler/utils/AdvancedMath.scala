package euler.utils

/**
  * @author guido
  */
object AdvancedMath {

  /* Binomial number */

  def binomial(n: Long, k: Long): Long = pascalTriangle(n)(n - k toInt)(k toInt)

  private def pascalTriangle(n: Long): Array[Array[Long]] = {
    val res = new Array[Array[Long]](n + 1 toInt)
    for (cn <- 0L until n) {
      res(cn toInt) = new Array[Long](n + 1L - cn toInt)
      for (ck <- 0L until (n + 1L - cn)) {
        res(cn toInt)(ck toInt) =
          if (cn == 0L || ck == 0L) 1L
          else res(cn - 1L toInt)(ck toInt) + res(cn toInt)(ck - 1L toInt)
      }
    }
    res
  }

  /* Factorial */

  def factorial(n: BigInt): BigInt = factorial(n, 1)

  private def factorial(n: BigInt, acc: BigInt): BigInt =
    if(n == 1) acc
    else factorial(n - 1, acc * n)

  /* Primes */

  private def isPrime(n: BigInt, previousPrimes: Set[BigInt]) = !previousPrimes.exists(p => n % p == 0)

  private def isPrime(n: Long, previousPrimes: Set[Long]) = !previousPrimes.exists(p => n % p == 0)

  private def primes(from: BigInt, previousPrimes: Set[BigInt]): Stream[BigInt] =
    if(isPrime(from, previousPrimes)) from #:: primes(from + 2, previousPrimes + from)
    else primes(from + 2, previousPrimes)

  val primes: Stream[BigInt] = 2 #:: primes(3, Set(2))

  def primesUntil(n: Long): Stream[Long] = 2 #:: primesUntil(3, n, Set(2))

  private def primesUntil(from: Long, n: Long, previousPrimes: Set[Long]): Stream[Long] =
    if (from > n) Stream.empty[Long]
    else if (isPrime(from, previousPrimes)) from #:: primesUntil(from + 2, n, previousPrimes + from)
    else primesUntil(from + 2, n, previousPrimes)

  /* Factors */
  def factors(n: Long): Seq[Long] = factors(n, 1, 0, Seq.empty)

  private def factors(n: Long, i: Long, prev: Long, acc: Seq[Long]): Seq[Long] =
    if(n % i == 0)
      if(n / i == i) acc :+ i
      else if(n / i == prev) acc
      else factors(n, i + 1, i, acc :+ i :+ n / i)
    else factors(n, i + 1, prev, acc)


  /* Prime Factors */

  type PrimeFactor = (BigInt, BigInt)

  def primeFactors(n: BigInt): Seq[PrimeFactor] = primeFactors(n, 2, Seq())

  private def primeFactors(n: BigInt, current: BigInt, result: Seq[PrimeFactor]): Seq[PrimeFactor] = {
    def stream(v: BigInt): Stream[BigInt] = v #:: stream(v + 1)

    stream(current).takeWhile(i => i <= n.^(2)).find(i => {
      n % i == 0
    }) match {
      case Some(i) =>
        var res = n
        var pow = BigInt(0)
        do {
          res = res / i
          pow = pow + 1
        } while (res % i == 0)
        primeFactors(res, i + 1, result :+ (i, pow))

      case None => result :+ (n, BigInt(1))
    }
  }

  /* Divisors */

  def properDivisors(n: Long): Seq[Long] = properDivisors(n, 1, Seq())

  private def properDivisors(n: Long, div: Long, acc: Seq[Long]): Seq[Long] =
    if(div >= n) acc
    else if(n % div == 0) properDivisors(n, div + 1, acc :+ div)
    else properDivisors(n, div + 1, acc)

  def properDivisors(n: Int): Seq[Int] = properDivisors(n, 1, Seq())

  private def properDivisors(n: Int, div: Int, acc: Seq[Int]): Seq[Int] =
    if(div >= n) acc
    else if(n % div == 0) properDivisors(n, div + 1, acc :+ div)
    else properDivisors(n, div + 1, acc)



}
