package euler.problems

import euler.utils.{Primes, Problem}

import scala.collection.mutable

/**
  * Problem 49
  *
  * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
  * (i) each of the three terms are prime, and,
  * (ii) each of the 4-digit numbers are permutations of one another.
  *
  * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
  * but there is one other 4-digit increasing sequence.
  *
  * What 12-digit number do you form by concatenating the three terms in this sequence?
  */
object Problem49 extends Problem with App {

  private def fourDigitPrimes: Stream[Int] = Primes.int.stream
    .dropWhile(n => n < 1000)
    .takeWhile(n => n < 10000)

  private def primePermutations(n: Int): List[Int] = n.toString
    .permutations
    .map(_.toInt)
    .filter(_ > 1000)
    .filter(Primes.int.isPrime)
    .toList

  private def arithmeticSequences(ns: List[Int]): List[Seq[Int]] = ns match {
    case Nil => List.empty[Seq[Int]]
    case n1 :: nss => nss
      .map(n2 => Seq(n1, n2, n2 + n2 - n1))
      .filter(ss => ns.contains(ss(2))) match {
      case Nil => arithmeticSequences(nss)
      case res => res ++: arithmeticSequences(nss)
    }
  }


  override def solution(): Any = fourDigitPrimes
    .map(primePermutations)
    .filter(_.length >= 3)
    .map(_.sorted)
    .flatMap(arithmeticSequences)
    .filterNot(_.isEmpty)
    .filterNot(_.head == 1487)
    .head
    .foldLeft("")(_ + _)
}
