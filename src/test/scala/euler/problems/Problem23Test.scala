package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem23Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem23.solution() == 4179871)
  }
}
