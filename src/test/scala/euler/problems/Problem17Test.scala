package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem17Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem17.solution() == 21124)
  }
}
