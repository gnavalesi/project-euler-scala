package euler.problems

import euler.utils.{AdvancedMath, Problem}

/**
  * @author guido
  */
object Problem24 extends Problem {
  def permutation(n: Int): Long = {
    calculateDigit(0, n, Nil, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)).map(_.toString).reduce(_ + _).toLong
  }

  def calculateDigit(n: Int, total: Int, acc: List[Int], remaining: List[Int]): List[Int] = {
    if(n == 9) acc ++ remaining
    else {
      val permutations = AdvancedMath.factorial(9 - n).toInt
      val i = total / permutations
      calculateDigit(n + 1, total - i * permutations, acc :+ remaining(i), remaining.slice(0, i) ++ remaining.slice(i + 1, remaining.length))
    }
  }

  override def solution(): Any = permutation(999999)
}
