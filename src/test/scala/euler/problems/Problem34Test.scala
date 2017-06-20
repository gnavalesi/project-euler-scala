package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem34Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem34.solution() == 40730)
  }
}
