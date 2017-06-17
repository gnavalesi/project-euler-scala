package euler.problems

/**
  * @author guido
  */
object Problem30 extends App {
  def sumOfPowersOfDigits(n: Int): Int = n.toString.split("").map(c => Math.pow(c.toInt.toDouble, 5).toInt).sum

  val maxDigit = Math.pow(9, 5).toInt

  def canBeWrittenAsSum(n: Int): Boolean =
    if(n.toString.length * maxDigit < n) false
    else sumOfPowersOfDigits(n) == n

  val nums = for {
    i <- Stream.from(2).takeWhile(n => n.toString.length * maxDigit >= n) if canBeWrittenAsSum(i)
  } yield i

  println(nums.sum)
}
