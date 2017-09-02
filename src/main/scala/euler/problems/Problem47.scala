package euler.problems

import euler.utils.{Primes, Problem}

/**
  * Problem 47
  *
  * The first two consecutive numbers to have two distinct prime factors are:
  * 14 = 2 × 7
  * 15 = 3 × 5
  *
  * The first three consecutive numbers to have three distinct prime factors are:
  *
  * 644 = 2² × 7 × 23
  * 645 = 3 × 5 × 43
  * 646 = 2 × 17 × 19.
  *
  * Find the first four consecutive integers to have four distinct prime factors each.
  * What is the first of these numbers?
  *
  */
object Problem47 extends Problem with App {

  private def tuples[T](ns: Stream[T], previous: Seq[T]): Stream[Seq[T]] = ns match {
    case Stream.Empty => Stream.Empty
    case _ => val value = previous.tail ++ Seq(ns.head)
      value #:: tuples(ns.tail, value)
  }

  private def tuples[T](ns: Stream[T]): Stream[Seq[T]] = {
    val previous = Seq(ns.head, ns.tail.head, ns.tail.tail.head, ns.tail.tail.tail.head)

    previous #:: tuples(ns.drop(4), previous)
  }

  private case class NumberAndPrimeFactors(n: Int, primeFactors: Seq[(Int, Int)])

  override def solution(): Any = tuples(Stream.from(2)
    .map(n => NumberAndPrimeFactors(n, Primes.int.primeFactors(n)))
    .filter(_.primeFactors.length == 4))
    .filter(npfs => {
      npfs.head.n + 3 == npfs(3).n
    })
    .head.head.n

}
