package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem32Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem32.solution() == 45228)
  }
}
