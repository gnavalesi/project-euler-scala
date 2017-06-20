package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem22Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem22.solution() == 871198282)
  }
}
