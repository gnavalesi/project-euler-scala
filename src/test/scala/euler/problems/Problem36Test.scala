package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem36Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem36.solution() == 872187)
  }
}
