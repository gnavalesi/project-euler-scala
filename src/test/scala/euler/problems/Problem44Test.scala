package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem44Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem44.solution() == 5482660)
  }
}
