package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem5Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem5.solution() == 232792560)
  }
}
