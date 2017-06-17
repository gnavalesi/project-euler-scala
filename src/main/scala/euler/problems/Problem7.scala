package euler.problems

import euler.utils.Primes

/**
  * @author guido
  */
object Problem7 extends App {
  println(Primes.int.stream.drop(10000).head)
}
