package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem32 extends Problem {
  def isPandigital(n: Int): Boolean = isPandigital(n.toString)

  private def isPandigital(str: String): Boolean = isPandigital(str, str.split(""))

  private def isPandigital(str: String, chars: Array[String]): Boolean =
    str.length == chars.distinct.length &&
      chars.map(_.toInt).sorted.eq(Stream.from(1).take(str.length).toArray)

  private def hasUniqueChars(str: String): Boolean =
    str.length == str.split("").distinct.length

  private lazy val pan = for {
    n <- 1234 until 100000
    if hasUniqueChars(n.toString) && AdvancedMath.properDivisorsFrom(n, 2).exists(d => {
      val m = n / d
      val str = m.toString + n.toString + d.toString
      str.length == 9 && !str.contains('0') && hasUniqueChars(str)
    })
  } yield n

  override def solution(): Any = pan.sum
}
