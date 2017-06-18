package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem2Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem2.solution() == 4613732)
  }
}
