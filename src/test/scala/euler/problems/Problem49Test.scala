package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem49Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem49.solution() == "296962999629")
  }
}
