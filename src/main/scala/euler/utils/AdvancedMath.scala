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

  /* Fibonacci */
  private def fibonacci(a: BigInt, b: BigInt): Stream[BigInt] = (a + b) #:: fibonacci(a + b, a)

  val fibonacci: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: fibonacci(BigInt(1), BigInt(1))

  /* Factorial */

  def factorial(n: BigInt): BigInt = factorial(n, 1)

  private def factorial(n: BigInt, acc: BigInt): BigInt =
    if (n == 1 || n == 0) acc
    else factorial(n - 1, acc * n)

  /* Factors */
  def factors(n: Long): Seq[Long] = factors(n, 1, 0, Seq.empty)

  private def factors(n: Long, i: Long, prev: Long, acc: Seq[Long]): Seq[Long] =
    if (n % i == 0)
      if (n / i == i) acc :+ i
      else if (n / i == prev) acc
      else factors(n, i + 1, i, acc :+ i :+ n / i)
    else factors(n, i + 1, prev, acc)

  def factorsNew(n: Long): Seq[Long] = (1L until n + 1).filter(n % _ == 0)

  /* Prime Factors */

  def primeFactors(n: BigInt): Seq[(BigInt, BigInt)] =
    Primes.bigInt.stream
      .takeWhile(p => p <= n)
      .filter(p => n % p == 0)
      .map(p => (p, BigInt(Stream.from(1).dropWhile(e => n % (p pow e) == 0).head - 1)))

  def primeFactors(n: Int): Seq[(Int, Int)] =
    Primes.int.stream
      .takeWhile(p => p <= n)
      .filter(p => n % p == 0)
      .map(p => (p, Stream.from(1).dropWhile(e => n % Math.pow(p, e) == 0).head - 1))

  def primeFactors(n: Long): Seq[(Long, Long)] =
    Primes.long.stream
      .takeWhile(p => p <= n)
      .filter(p => n % p == 0)
      .map(p => (p, (Stream.from(1).dropWhile(e => n % Math.pow(p, e) == 0).head - 1).toLong))

//  def primeFactors(n: Int): Seq[(Int, Int)] = (for {
//    p <- Primes.int.stream.takeWhile(i => i <= n)
//  } yield (p, Stream.from(1).dropWhile(e => n % Math.pow(p, e) == 0).head - 1)).filter((a: (Int, Int)) => a._2 > 0)
//
//  def primeFactors(n: Long): Seq[(Int, Int)] = (for {
//    p <- Primes.int.stream.takeWhile(i => i <= n)
//  } yield (p, Stream.from(1).dropWhile(e => n % Math.pow(p, e) == 0).head - 1)).filter((a: (Int, Int)) => a._2 > 0)

  /* Divisors */

  def properDivisors(n: Long): Seq[Long] = properDivisors(n, 1, Seq())

  private def properDivisors(n: Long, div: Long, acc: Seq[Long]): Seq[Long] =
    if (div >= n) acc
    else if (n % div == 0) properDivisors(n, div + 1, acc :+ div)
    else properDivisors(n, div + 1, acc)

  def properDivisors(n: Int): Seq[Int] = properDivisors(n, 1, Seq())

  private def properDivisors(n: Int, div: Int, acc: Seq[Int]): Seq[Int] =
    if (div >= n) acc
    else if (n % div == 0) properDivisors(n, div + 1, acc :+ div)
    else properDivisors(n, div + 1, acc)

  def properDivisorsFrom(n: Int, from: Int): Seq[Int] = properDivisors(n, from, Seq())


}
