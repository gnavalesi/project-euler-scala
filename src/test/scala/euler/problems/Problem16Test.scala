package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem16Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem16.solution() == 1366)
  }
}
