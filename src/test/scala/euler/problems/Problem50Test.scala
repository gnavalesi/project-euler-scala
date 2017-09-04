package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem50Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem50.solution() == 997651)
  }
}
