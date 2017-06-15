package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
// FIXME
object Problem15 extends App {
  val width = 20
  val height = 20

  def latticePaths(whs: Seq[(Int, Int)], acc: Int): Int = whs match {
    case Nil => acc
    case wh :: whss => wh match {
      case (w, h) if w == width && h == height => latticePaths(whss, acc + 1)
      case (w, h) if w == width => latticePaths((width, h + 1) :: whss, acc)
      case (w, h) if h == height => latticePaths((w + 1, height) :: whss, acc)
      case (w, h) => latticePaths((w + 1, h) :: (w, h + 1) :: whss, acc)
    }
  }

  println(AdvancedMath.binomial(height + width, height))

}
