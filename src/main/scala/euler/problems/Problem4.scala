package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem4 extends Problem {
  private def multiples(a: Long, b: Long): Stream[Long] = if(b == 100) {
    if(a == 100) (100 * 100) #:: Stream.empty[Long]
    else (a * 100) #:: multiples(a - 1, 999)
  } else (a * b) #:: multiples(a, b - 1)

  private def isPalindrome(num: Long): Boolean = {
    val string = num.toString
    (0 until (string.length / 2).floor.toInt) forall(i => string.charAt(i) == string.charAt(string.length - 1 - i))
  }

  def solution(): Long = multiples(999, 999).filter(isPalindrome).max
}
