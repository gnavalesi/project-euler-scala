package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem51Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem51.solution() == 121313)
  }
}
