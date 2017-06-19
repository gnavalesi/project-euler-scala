package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem8Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem8.solution() == 23514624000L)
  }
}
