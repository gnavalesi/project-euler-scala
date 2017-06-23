package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem39Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem39.solution() == 840)
  }
}
