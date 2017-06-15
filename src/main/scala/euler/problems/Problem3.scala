package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem3 extends App {
  val n: Long = 600851475143L
  val primes = AdvancedMath.primeFactors(n)
  println(primes.last._1)
}
