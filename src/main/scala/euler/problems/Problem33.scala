package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem33 extends App {
  def sharedDigits(a: Int, b: Int): Seq[Char] = a.toString.filter(b.toString.contains(_)).distinct

  def sameFraction(n1: Int, d1: Int, n2: Int, d2: Int): Boolean = n1 * d2 == n2 * d1

  def simplify(n: Int, d: Int): (Int, Int) = {
    val nFactors = (for {
      f <- AdvancedMath.primeFactors(n)
    } yield f._1.toInt -> f._2.toInt).toMap

    val rFactors = (for {
      f <- AdvancedMath.primeFactors(d)
    } yield f._1.toInt -> {
      if (nFactors.isDefinedAt(f._1.toInt)) Math.abs(f._2.toInt - nFactors(f._1.toInt))
      else f._2.toInt
    }).filter(p => p._2 > 0)

    val denominator = rFactors.map(p => Math.pow(p._1, p._2)).product.toInt

    (denominator * n / d, denominator)
  }


  val fractions = for {
    d <- 10 until 100
    n <- 10 until d
    if n % 10 != 0 || d % 10 != 0
    sds = sharedDigits(n, d)
    sd <- sds
    nn = n.toString.replaceFirst("" + sd, "").toInt
    nd = d.toString.replaceFirst("" + sd, "").toInt
    if nd > 0
    if sameFraction(n, d, nn, nd)
  } yield (n, d)

  val product = fractions.reduce((f1, f2) => (f1._1 * f2._1, f1._2 * f2._2))

  println(simplify(product._1, product._2))


}
