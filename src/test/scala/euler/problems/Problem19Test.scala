package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem19Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem19.solution() == 171)
  }
}
