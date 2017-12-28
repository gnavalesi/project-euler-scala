package euler.problems

import euler.utils.{Primes, Problem}

/**
  * Problem 58
  *
  * Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
  * (37)  36   35   34   33   32  (31)
  * 38  (17)  16   15   14  (13)  30
  * 39   18  ( 5)   4  ( 3)  12   29
  * 40   19    6    1    2   11   28
  * 41   20  ( 7)   8    9   10   27
  * 42   21   22   23   24   25   26
  * (43)  44   45   46   47   48   49
  *
  * It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is
  * that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
  *
  * If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
  * If this process is continued, what is the side length of the square spiral for which the ratio of primes along both
  * diagonals first falls below 10%?
  * *
  **/
object Problem58 extends Problem with App {

  case class Square(sideLength: Int, corners: (Int, Int, Int, Int))

  def squaresStream(): Stream[Square] = squaresStream(Square(1, (1, 1, 1, 1)))

  def squaresStream(square: Square): Stream[Square] = {
    val newURCorner = square.corners._4 + square.sideLength + 1
    val newULCorner = newURCorner + square.sideLength + 1
    val newDLCorner = newULCorner + square.sideLength + 1
    val newDRCorner = newDLCorner + square.sideLength + 1
    val value = Square(square.sideLength + 2, (newURCorner, newULCorner, newDLCorner, newDRCorner))

    value #:: squaresStream(value)
  }

  def countPrimes(n: Product): Int = n.productIterator.count(n => Primes.int.isPrime(n.asInstanceOf[Int]))

  def findLevel(primesCount: Int, squares: Stream[Square]): Int = {
    val primes = primesCount + countPrimes(squares.head.corners)
    if(primes * 1.0 / (squares.head.sideLength * 2 - 1) < 0.1) squares.head.sideLength
    else findLevel(primes, squares.tail)
  }

  override def solution(): Any = findLevel(0, squaresStream())

  println(solution())
}
