package euler.problems

/**
  * @author guido
  */
object Problem6 extends App {
  val sums = (1L until 101L).foldLeft((0L, 0L))((acc: (Long, Long), n: Long) => {
    (acc._1 + Math.pow(n, 2).toLong, acc._2 + n)
  })

  val diff = Math.pow(sums._2, 2).toLong - sums._1

  println(diff)
}
