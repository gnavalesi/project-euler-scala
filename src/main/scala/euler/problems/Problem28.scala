package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem28 extends Problem {
  def corners(start: Int, step: Int): Stream[Int] =
    (start + step) #:: (start + step * 2) #:: (start + step * 3) #:: (start + step * 4) #:: corners(start + step * 4,
      step + 2)

  def corners: Stream[Int] = 1 #:: corners(1, 2)

  private lazy val max = 1001 * 1001
  private lazy val allCorners = corners.takeWhile(_ <= max)

  override def solution(): Any = allCorners.sum
}
