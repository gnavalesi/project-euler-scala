package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem7Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem7.solution() == 104743)
  }
}
