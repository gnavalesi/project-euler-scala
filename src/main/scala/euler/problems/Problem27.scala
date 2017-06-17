package euler.problems

import java.lang.Math._

import euler.utils.Primes

/**
  * @author guido
  */
object Problem27 extends App {
  def evaluate(n: Int, a: Int, b: Int): Int = pow(n, 2).toInt + a * n + b

  def consecutivePrimes(a: Int, b: Int, n: Int, acc: Seq[Int]): Seq[Int] = {
    if((n % 2 == 0 && b % 2 == 0) || (n % 2 != 0 && ((a % 2 == 0 && b % 2 != 0) || (a % 2 != 0 && b % 2 == 0))))
      consecutivePrimes(a, b, n + 1, acc)
    else {
      val res = evaluate(n, a, b)
      // This should be done more efficiently, looking for primes following the curve of the function
      val properPrimes = Primes.int.stream.dropWhile(_ < res)
      if(properPrimes.head == res) consecutivePrimes(a, b, n + 1, acc :+ res)
      else acc
    }

  }

  def consecutivePrimes(a: Int, b: Int): Seq[Int] = {
    if(b % 2 == 0 && a % 2 != 0) Seq.empty[Int]
    else consecutivePrimes(a, b, 1, Seq.empty[Int])
  }

  val evaluations = for {
    a <- Stream.from(-999).takeWhile(_ < 1000)
    // b is the first result since n == 0, so b must be prime
    b <- Primes.int.stream.takeWhile(_ < 1001)
    cp = consecutivePrimes(a, b.toInt) if cp.nonEmpty
  } yield {
    println(a, b, cp.length)
    (a * b, cp.length)
  }

  val max = evaluations.maxBy(_._2)

  println(max)

}
