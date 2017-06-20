package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem26Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem26.solution() == 983)
  }
}
