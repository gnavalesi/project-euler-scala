package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem17 extends Problem {
  val units: Map[Int, Int] = Map(0 -> 0, 1 -> 3, 2 -> 3, 3 -> 5, 4 -> 4, 5 -> 4, 6 -> 3, 7 -> 5, 8 -> 5, 9 -> 4)

  val tens: Map[Int, Int] = Map(10 -> 3, 11 -> 6, 12 -> 6, 13 -> 8, 14 -> 8, 15 -> 7, 16 -> 7, 17 -> 9, 18 -> 8,
    19 -> 8)

  val decenes: Map[Int, Int] = Map(20 -> 6, 30 -> 6, 40 -> 5, 50 -> 5, 60 -> 5, 70 -> 7, 80 -> 6, 90 -> 6)
  val hundred: Int = 7

  val and: Int = 3

  val thousand: Int = 8

  def value(n: Int): Int = n match {
    case i if i < 10 => units(i)
    case i if i < 20 => tens(i)
    case i if i < 100 => decenes((i / 10) * 10) + units(i % 10)
    case i if i < 1000 => units(i / 100) + hundred +
      (if (i % 100 != 0) and + value(n % 100)
      else 0)
    case 1000 => units(1) + thousand
  }

  def solution(): Int = (1 until 1001).map(value).sum
}
