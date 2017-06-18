package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem3Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem3.solution() == 6857)
  }
}
