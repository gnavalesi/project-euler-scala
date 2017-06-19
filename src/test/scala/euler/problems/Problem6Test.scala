package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem6Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem6.solution() == 25164150)
  }
}
