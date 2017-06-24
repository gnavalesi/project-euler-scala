package euler.problems

import euler.utils.{Primes, Problem}

/**
  * @author guido
  */
object Problem41 extends Problem with App {

  private def pandigitalNumbers(n: Int): Stream[Int] =
    (1 until (n + 1)).permutations.map(_.mkString.toInt).toStream #:::
      (if(n == 1) Stream.empty[Int] else pandigitalNumbers(n - 1))

  private def pandigitalNumbers(): Stream[Int] =  pandigitalNumbers(9)

  override def solution(): Int = pandigitalNumbers().sorted.reverse.dropWhile(!Primes.int.isPrime(_)).head

  println(solution())
}
