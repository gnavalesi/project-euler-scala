package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem37Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem37.solution() == 748317)
  }
}
