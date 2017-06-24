package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem40Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem40.solution() == 210)
  }
}
