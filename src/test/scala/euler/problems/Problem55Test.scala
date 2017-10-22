package euler.problems

import euler.problems.Problem55._
import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem55Test extends FlatSpec {
  it should "10677 is Lychrel" in {
    assert(isLychrel(BigInt(10677)))
  }

  it should "return the correct answer" in {
    assert(solution() == 249)
  }
}
