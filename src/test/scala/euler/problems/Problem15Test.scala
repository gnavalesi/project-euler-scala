package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem15Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem15.solution() == 137846528820L)
  }
}
