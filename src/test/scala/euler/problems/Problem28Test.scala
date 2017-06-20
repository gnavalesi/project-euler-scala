package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem28Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem28.solution() == 669171001)
  }
}
