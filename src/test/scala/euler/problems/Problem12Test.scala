package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem12Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem12.solution() == 76576500)
  }
}
