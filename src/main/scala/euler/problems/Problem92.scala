package euler.problems

import euler.utils.Problem

import scala.collection.mutable

/**
  * Problem 92
  *
  * A number chain is created by continuously adding the square of the digits in a number to form a new number until it
  * has been seen before.
  *
  * For example,
  * 44 → 32 → 13 → 10 → 1 → 1
  * 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
  *
  * Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY
  * starting number will eventually arrive at 1 or 89.
  *
  * How many starting numbers below ten million will arrive at 89?
  *
  **/
object Problem92 extends Problem with App {

  val endsIn89Values: mutable.Map[Int, Boolean] = mutable.Map()

  def chain(n: Int): Stream[Int] = n #:: chain(n.toString.split("").map(_.toInt).map(a => a * a).sum)

  def endsIn89(chain: Stream[Int]): Boolean = endsIn89Values.getOrElseUpdate(chain.head,
    if (chain.head == 89) true
    else if (chain.head == 1) false
    else endsIn89(chain.tail))

  override def solution(): Any = Stream.from(1).take(10000000)
    .map(chain)
    .count(endsIn89)

  println(solution())
}
