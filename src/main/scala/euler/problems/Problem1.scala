package euler.problems

/**
  * @author guido
  */
object Problem1 extends App {
  val from = 1
  val to = 1000
  val isDivisible = (i: Int) => i % 3 == 0 || i % 5 == 0

  val sum: Integer = (from until to by 1) filter isDivisible reduce(_ + _)

  println(sum)
}
