package euler.problems

import org.scalatest.FlatSpec

/**
  * @author guido
  */
class Problem21Test extends FlatSpec {
  it should "return the correct answer" in {
    assert(Problem21.solution() == 31626)
  }
}
