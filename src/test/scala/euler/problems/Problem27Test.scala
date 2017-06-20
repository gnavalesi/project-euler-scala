package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem27Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem27.solution() == -59231)
  }
}
