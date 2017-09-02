package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem46Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem46.solution() == 5777)
  }
}
