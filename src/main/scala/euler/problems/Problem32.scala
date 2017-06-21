package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem32 extends Problem {
  private def isPandigital(n: Int): Boolean = isPandigital(n.toString)

  private def isPandigital(str: String): Boolean = isPandigital(str, str.split(""))

  private def isPandigital(str: String, chars: Array[String]): Boolean =
    str.length == chars.distinct.length &&
      chars.map(_.toInt).sorted.eq(Stream.from(1).take(str.length).toArray)

  private def hasUniqueChars(str: String): Boolean =
    str.length == str.split("").distinct.length

  private def permutations(elems: Seq[Int]): Stream[Seq[Int]] = elems match {
    case Nil => Stream.empty[Seq[Int]]
    case x :: Nil => Stream(Seq(x))
    case _ => elems.toStream
      .flatMap(elem => permutations(elems.filterNot(_ == elem))
        .map(permutation => elem +: permutation))
  }

  private val digits = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  private def seqToInt(seq: Seq[Int]): Int = seq.reverse.zip(Stream.from(0)).map(
    r => r._1 * Math.pow(10, r._2)).sum.toInt

//  println(permutations(digits)
//    .flatMap(p => Stream(
//      Seq(p.slice(0, 4), p.slice(4, 6), p.slice(6, 9)),
//      Seq(p.slice(0, 4), p.slice(4, 7), p.slice(7, 9)))).mkString)

  private case class Product(res: Int, a: Int, b: Int)

//  permutations(digits)
//    .flatMap(p => Seq(
//      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 6)), seqToInt(p.slice(6, 9))),
//      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 7)), seqToInt(p.slice(7, 9)))))
//    .groupBy(p => p.res)
//    .filter(p => p._2.exists(product => product.a * product.b == product.res))
//    .keys
//    .sum

  private lazy val pan = for {
    n <- 1234 until 100000
    if hasUniqueChars(n.toString) && AdvancedMath.properDivisorsFrom(n, 2).exists(d => {
      val m = n / d
      val str = m.toString + n.toString + d.toString
      str.length == 9 && !str.contains('0') && hasUniqueChars(str)
    })
  } yield n

  override def solution(): Any = permutations(digits)
    .flatMap(p => Seq(
      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 7)), seqToInt(p.slice(7, 9))),
      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 8)), seqToInt(p.slice(8, 9)))))
    .groupBy(p => p.res)
    .filter(p => p._2.exists(product => product.a * product.b == product.res))
    .keys
    .sum

//    pan.sum
}
