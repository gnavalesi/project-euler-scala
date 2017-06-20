package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem20Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem20.solution() == 648)
  }
}
