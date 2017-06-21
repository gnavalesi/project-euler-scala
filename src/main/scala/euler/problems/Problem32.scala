package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem32 extends Problem {

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

  private case class Product(res: Int, a: Int, b: Int)

  override def solution(): Any = permutations(digits)
    .flatMap(p => Seq(
      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 7)), seqToInt(p.slice(7, 9))),
      Product(seqToInt(p.slice(0, 4)), seqToInt(p.slice(4, 8)), seqToInt(p.slice(8, 9)))))
    .groupBy(p => p.res)
    .filter(p => p._2.exists(product => product.a * product.b == product.res))
    .keys
    .sum
}
