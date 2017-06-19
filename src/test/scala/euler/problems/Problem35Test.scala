package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem35Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem35.solution() == 55)
  }
}
