package euler.problems

import euler.problems.Problem92._
import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem92Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(solution() == 8581146)
  }
}
