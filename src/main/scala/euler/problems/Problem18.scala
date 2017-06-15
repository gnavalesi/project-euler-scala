package euler.problems

/**
  * @author guido
  */
object Problem18 extends App {
  type Triangle = List[List[Int]]

  val triangle = List[List[Int]](
    List[Int]( 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23),
      List[Int](63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
        List[Int](91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
          List[Int](70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
            List[Int](53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
              List[Int](41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
                List[Int](41, 41, 26, 56, 83, 40, 80, 70, 33),
                  List[Int](99, 65,  4, 28,  6, 16, 70, 92),
                    List[Int](88,  2, 77, 73,  7, 63, 67),
                      List[Int](19,  1, 23, 75,  3, 34),
                        List[Int](20,  4, 82, 47, 65),
                          List[Int](18, 35, 87, 10),
                            List[Int](17, 47, 82),
                              List[Int](95, 64),
                                List[Int](75))

  def largestSum(): Int = {
    largestSum(triangle)
  }

  def diminish(values: List[Int]): List[Int] = values.zip(values.tail).map(p => p._1 max p._2)

  def sum(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a :: ass, b :: bss) => (a + b) :: sum(ass, bss)
  }

  def largestSum(tr: Triangle): Int = tr match {
    case head :: Nil => head.head
    case head :: neck :: tail => largestSum(sum(diminish(head), neck) :: tail)
  }

  println(largestSum())
}
