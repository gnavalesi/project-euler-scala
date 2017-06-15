package euler.problems

/**
  * @author guido
  */
object Problem16 extends App {
  val pow = BigInt(2).pow(1000)

  val sum = pow.toString.split("").map(_.toInt).sum
  println(sum)
}
