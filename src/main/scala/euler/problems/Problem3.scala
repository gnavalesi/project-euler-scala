package euler.problems

import euler.utils.{AdvancedMath, Primes, Problem}

/**
  * @author guido
  */
object Problem3 extends Problem {
  val n: Long = 600851475143L

//  def solution(): BigInt = AdvancedMath.primeFactors(n).maxBy(_._1)._1

  def solution(): Long = Primes.long.stream.takeWhile(p => p * p <= n).filter(p => n % p == 0).max
}
