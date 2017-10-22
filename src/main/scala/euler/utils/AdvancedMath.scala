package euler.utils

/**
  * @author guido
  */
object AdvancedMath {

  /* Binomial number */

  def binomial(n: Int, k: Int): Long = pascalTriangle(n)(n - k)(k)

  private def pascalTriangle(n: Int): Array[Array[Long]] = {
    val res = new Array[Array[Long]](n + 1)
    for (row <- 0 until n + 1) {
      res(row) = new Array[Long](n + 1 - row)
      for (column <- 0 until (n + 1 - row)) {
        res(row)(column) =
          if (row == 0 || column == 0) 1
          else res(row - 1)(column) + res(row)(column - 1)
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

  def factorial(n: Long): Long = factorial(n, 1)

  private def factorial(n: Long, acc: Long): Long =
    if (n == 1 || n == 0) acc
    else factorial(n - 1, acc * n)

  def factorial(n: Int): Int = factorial(n, 1)

  private def factorial(n: Int, acc: Int): Int =
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
