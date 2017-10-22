package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem53Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem53.solution() == 4075)
  }
}
