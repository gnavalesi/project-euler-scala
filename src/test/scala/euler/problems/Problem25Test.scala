package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem25Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem25.solution() == 4782)
  }
}
