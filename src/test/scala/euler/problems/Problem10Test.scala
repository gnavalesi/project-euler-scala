package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem10Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem10.solution() == 142913828922L)
  }
}
