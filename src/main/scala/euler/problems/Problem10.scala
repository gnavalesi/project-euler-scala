package euler.problems

import euler.utils.Primes

/**
  * @author guido
  */
object Problem10 extends App {
  val sum = Primes.long.stream.takeWhile(_ <= 2000000).sum

  println(sum)
}
