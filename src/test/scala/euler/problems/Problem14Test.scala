package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem14Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem14.solution() == 837799)
  }
}
