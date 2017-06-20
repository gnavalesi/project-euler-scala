package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem33Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem33.solution() == 100)
  }
}
