package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem33 extends Problem {

  private case class Fraction(numerator: Int, denominator: Int)

  private def fractionsStream: Stream[Fraction] =
    Stream.from(10)
      .takeWhile(_ < 100)
      .filter(_ % 10 != 0)
      .flatMap(denominator =>
        Stream.from(10)
          .takeWhile(numerator => numerator < denominator)
          .filter(_ % 10 != 0)
          .map(numerator => Fraction(numerator, denominator)))

  private def findSharedDigits(fraction: Fraction): String =
    fraction.numerator.toString.intersect(fraction.denominator.toString)

  private def sameFraction(fraction1: Fraction, fraction2: Fraction): Boolean =
    fraction1.numerator * fraction2.denominator == fraction2.numerator * fraction1.denominator

  private def wrongSimplify(fraction: Fraction, sharedDigit: Char): Fraction =
    Fraction(
      fraction.numerator.toString.replaceFirst("" + sharedDigit, "").toInt,
      fraction.denominator.toString.replaceFirst("" + sharedDigit, "").toInt)

  private def wrongSimplify(fraction: Fraction, sharedDigits: String): Seq[Fraction] =
    sharedDigits.toCharArray.map(sharedDigit => wrongSimplify(fraction, sharedDigit))

  private def simplify(fraction: Fraction): Fraction = {
    Primes.int.stream
      .takeWhile(prime => prime * prime <= fraction.numerator)
      .foldLeft(fraction)((accumulator: Fraction, prime: Int) => {
        val exponent = Stream.from(1)
          .dropWhile(exponent => accumulator.numerator % Math.pow(prime, exponent) == 0
            && accumulator.denominator % Math.pow(prime, exponent) == 0)
          .head - 1
        if (accumulator.numerator % prime == 0)
          Fraction((accumulator.numerator / Math.pow(prime, exponent)).toInt, (accumulator.denominator / Math.pow(prime, exponent)).toInt)
        else accumulator
      })
  }

  private lazy val fractions = fractionsStream
    .map(fraction => (fraction, findSharedDigits(fraction)))
    .filter(pair => pair._2.nonEmpty)
    .map(pair => (pair._1, wrongSimplify(pair._1, pair._2)))
    .flatMap(pair => pair._2.map(simplification => (pair._1, simplification)))
    .filter(pair => sameFraction(pair._1, pair._2))
    .map(pair => pair._1)

  private lazy val product = fractions.reduce(
    (fraction1, fraction2) => Fraction(fraction1.numerator * fraction2.numerator,
      fraction1.denominator * fraction2.denominator))

  override def solution(): Any = simplify(product).denominator
}
