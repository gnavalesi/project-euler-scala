package euler.problems

/**
  * @author guido
  */
object Problem2 extends App {

  val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map {n => n._1 + n._2 }

  val sum = fibs.filter(_ % 2 == 0).takeWhile(n => n < 4000000).sum

  println(sum)
}
