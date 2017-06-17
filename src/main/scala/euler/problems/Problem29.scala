package euler.problems

/**
  * @author guido
  */
object Problem29 extends App {
  val values = (for {
    a <- 2 until 101
    b <- 2 until 101
  } yield BigInt(a).pow(b)).distinct.length

  println(values)
}
