package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem11Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem11.solution() == 70600674)
  }
}
