package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem41Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem41.solution() == 7652413)
  }
}
