package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem13Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem13.solution() == 5537376230L)
  }
}
