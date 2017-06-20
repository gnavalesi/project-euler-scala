package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem31Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem31.solution() == 73682)
  }
}
