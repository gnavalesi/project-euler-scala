package euler.problems

import euler.utils.Problem

import scala.io.Source

/**
  * @author guido
  */
object Problem42 extends Problem {
  private lazy val letters = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

  private lazy val triangleNumbers = triangleNumbersFunc(1)

  private def triangleNumbersFunc(n: Int): Stream[Int] = (n * (n + 1) / 2) #:: triangleNumbersFunc(n + 1)

  private def getWordValue(word: String): Int = word.split("").map(letters.indexOf(_) + 1).sum

  private def isTriangleNumber(n: Int): Boolean = triangleNumbers.dropWhile(_ < n).head == n

  override def solution(): Any = Source.fromResource("problem_42.txt").mkString.split(",")
    .map(_.replace("\"", ""))
    .map(getWordValue)
    .count(isTriangleNumber)
}
