package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem30Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem30.solution() == 443839)
  }
}
