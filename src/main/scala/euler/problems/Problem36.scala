package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem36 extends Problem {
  private def isPalindrome(str: String) = str.substring(0, str.length / 2) == str.substring((str.length + str.length % 2) / 2).reverse

  private def toBinary(n: Int): String = toBinary(n, "")

  private def toBinary(n: Int, acc: String): String =
    if(n < 2) n + acc
    else toBinary(n / 2, (n % 2) + acc)

  override def solution(): Any = Stream.from(1)
    .takeWhile(_ < 1000000)
    .filter(n => isPalindrome(n.toString) && isPalindrome(toBinary(n)))
    .sum
}
