package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem52Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem52.solution() == 142857)
  }
}
