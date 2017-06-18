package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem1Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem1.solution() == 233168)
  }
}
