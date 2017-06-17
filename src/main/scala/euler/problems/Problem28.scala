package euler.problems

/**
  * @author guido
  */
object Problem28 extends App {
  def corners(start: Int, step: Int): Stream[Int] =
    (start + step) #:: (start + step * 2) #:: (start + step * 3) #:: (start + step * 4) #:: corners(start + step * 4,
      step + 2)

  def corners: Stream[Int] = 1 #:: corners(1, 2)

  val max = 1001 * 1001
  val allCorners = corners.takeWhile(_ <= max)

  println(allCorners.sum)
}
