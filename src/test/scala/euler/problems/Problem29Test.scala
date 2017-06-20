package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem29Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem29.solution() == 9183)
  }
}
