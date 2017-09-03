package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem48Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem48.solution() == BigInt(9110846700L))
  }
}
