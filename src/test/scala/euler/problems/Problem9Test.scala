package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem9Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem9.solution() == 31875000)
  }
}
