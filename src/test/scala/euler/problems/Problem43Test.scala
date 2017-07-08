package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem43Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem43.solution() == 16695334890L)
  }
}
