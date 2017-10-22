package euler.problems

import euler.utils.Problem

/**
  * Problem 53
  *
  * There are exactly ten ways of selecting three from five, 12345:
  * 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
  *
  * In combinatorics, we use the notation, 5_C_3 = 10.
  *
  * In general,
  *
  * n_C_r = n! / r!(n−r)! ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
  *
  * It is not until n = 23, that a value exceeds one-million: 23_C_10 = 1144066.
  *
  * How many, not necessarily distinct, values of nCr, for 1 ≤ n ≤ 100, are greater than one-million?
  *
  **/
object Problem53 extends Problem with App {

  def pascalTriangleMillions(n: Int): Int = {
    var count = 0
    val res = new Array[Array[Long]](n + 1)
    for (row <- 0 until n + 1) {
      res(row) = new Array[Long](n + 1 - row)
      for (column <- 0 until (n + 1 - row)) {
        if (row == 0 || column == 0) {
          res(row)(column) = 1
        } else {
          if(res(row - 1)(column) < 1 || res(row)(column - 1) < 1) {
            res(row)(column) = 0
            count = count + 1
          } else {
            val value = res(row - 1)(column) + res(row)(column - 1)
            if(value > 1000000) {
              res(row)(column) = 0
              count = count + 1
            } else {
              res(row)(column) = value
            }
          }
        }
      }
    }
    count
  }

  override def solution(): Any = pascalTriangleMillions(100)
}
