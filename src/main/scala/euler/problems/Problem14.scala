package euler.problems

// FIXME
object Problem14 extends App {
  def collatz(n: Long): Stream[Long] = n #:: (n match {
    case 1 => Stream.empty[Long]
    case x if x % 2 == 0 => collatz(n / 2)
    case _ => collatz(3 * n + 1)
  })

  val maxCollatzLength = (1 until 1000000).toStream.map(n => (n, collatz(n).length)).reduce((a: (Int, Int), b: (Int, Int)) => if(a._2 > b._2) a else b)
  println(maxCollatzLength)
}