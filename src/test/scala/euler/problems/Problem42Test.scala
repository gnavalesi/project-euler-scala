package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem42Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem42.solution() == 162)
  }
}
