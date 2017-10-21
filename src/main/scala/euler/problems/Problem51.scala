package euler.problems

import euler.utils.{Primes, Problem}

/**
  * Problem 51
  *
  * By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
  * 13, 23, 43, 53, 73, and 83, are all prime.
  *
  * By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having
  * seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773,
  * and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
  *
  * Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same
  * digit, is part of an eight prime value family.
  **/
object Problem51 extends Problem with App {

  private def replace(value: Seq[Int], replacements: Seq[Option[Int]]): Seq[Int] =
    value.zip(replacements).map(f => f._2.getOrElse(f._1))

  private def replacementValues(replacementPattern: Seq[Option[_]]): Seq[Seq[Option[Int]]] =
    (0 until 10).map(v => replacementPattern.map(r => r.map(_ => v)))

  private def primeValueFamily(value: Seq[Int], replacementPattern: Seq[Option[_]]): Seq[Long] =
    replacementValues(replacementPattern)
      .map(rv => replace(value, rv))
      .map(_.mkString)
      .map(_.toLong)
      .filter(_ > Math.pow(10, value.length - 1))
      .filter(v => Primes.long.isPrime(v))


  private def primeValueFamily(value: Long, replacementPattern: Seq[Option[_]]): Seq[Long] =
    primeValueFamily(value.toString.split("").map(_.toInt).toSeq, replacementPattern)

  private def replacementPatterns(length: Int): Seq[Seq[Option[_]]] = (1 until length)
    .flatMap(n => ((0 until n).map(_ => Some()) ++ (n until length).map(_ => None))
      .permutations
      .filter(p => p.length == length))

  private def primeValueFamilies(value: Long): Seq[Seq[Long]] =
    replacementPatterns(value.toString.length)
      .map(rp => primeValueFamily(value, rp))
      .filter(_.nonEmpty)

  override def solution(): Any = Primes.long.stream
    .flatMap(p => primeValueFamilies(p))
    .find(pvf => pvf.length == 8)
    .get
    .head
}
