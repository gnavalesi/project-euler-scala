package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem24Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem24.solution() == 2783915460L)
  }
}
