package euler.problems

import euler.utils.{Primes, Problem}

import scala.collection.mutable

/**
  * Problem 48
  *
  * The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
  *
  * Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
  */
object Problem48 extends Problem with App {

  private lazy val powersCache: mutable.Map[BigInt, mutable.Map[BigInt, BigInt]] = mutable.Map.empty

  private def calculatePower(pf: (Int, Int)): BigInt = powersCache
    .getOrElseUpdate(pf._1, mutable.Map.empty)
    .getOrElseUpdate(pf._2, BigInt(pf._1).pow(pf._2).mod(BigInt(10000000000L)))


  override def solution(): Any =
    (Stream(BigInt(1)) +: (2 until 1001)
      .map(n => Primes.int.primeFactors(n)
        .map(pf => pf.copy(_2 = pf._2 * n))
        .map(calculatePower)))
      .map(pfs => pfs.reduce((a: BigInt, b: BigInt) => (a * b).mod(BigInt(10000000000L))))
      .reduce((a: BigInt, b: BigInt) => (a + b) % 10000000000L)

  println(solution())

}
