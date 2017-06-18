package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem4Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem4.solution() == 906609)
  }
}
