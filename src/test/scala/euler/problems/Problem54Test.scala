package euler.problems

import org.scalatest.FlatSpec

import Problem54._

/**
  * @author guido
  */
class Problem54Test extends FlatSpec {
  it should "pass the examples" in {
    assert(parseHand("5H 5C 6S 7S KD") < parseHand("2C 3S 8S 8D TD"))
    assert(parseHand("5D 8C 9S JS AC") > parseHand("2C 5C 7D 8S QH"))
    assert(parseHand("2D 9C AS AH AC") < parseHand("3D 6D 7D TD QD"))
    assert(parseHand("4D 6S 9H QH QC") > parseHand("3D 6D 7H QD QS"))
    assert(parseHand("2H 2D 4C 4D 4S") > parseHand("3C 3D 3S 9S 9D"))
  }

  it should "parse RoyalFlush" in {
    assert(parseHand("TS JS QS KS AS") match {
      case RoyalFlush(_) => true
      case _ => false
    })
  }

  it should "parse StraightFlush" in {
    assert(parseHand("9S TS JS QS KS") match {
      case StraightFlush(_) => true
      case _ => false
    })
  }

  it should "parse FourKind" in {
    assert(parseHand("9S TS 9H 9C 9D") match {
      case FourKind(_, _) => true
      case _ => false
    })
  }

  it should "parse FullHouse" in {
    assert(parseHand("TC TS 9H 9C 9D") match {
      case FullHouse(_, _, _) => true
      case _ => false
    })
  }

  it should "parse Flush" in {
    assert(parseHand("AS TS 9S 5S 2S") match {
      case Flush(_) => true
      case _ => false
    })
  }

  it should "parse Straight" in {
    assert(parseHand("9S 7S 8S 5H 6S") match {
      case Straight(_) => true
      case _ => false
    })
  }

  it should "parse ThreeKind" in {
    assert(parseHand("9S 7S 9H 9C 6S") match {
      case ThreeKind(_, _) => true
      case _ => false
    })
  }

  it should "parse DoublePair" in {
    assert(parseHand("9S 7S 9H 7C 6S") match {
      case DoublePair(_, _, _) => true
      case _ => false
    })
  }

  it should "parse Pair" in {
    assert(parseHand("9S 7S 9H 5C 6S") match {
      case Pair(_, _) => true
      case r => {
        println(r)
        false
      }
    })
  }

  it should "parse HighCard" in {
    assert(parseHand("9S 7S 4H 5C 6S") match {
      case HighCard(_) => true
      case _ => false
    })
  }

  it should "pass the firs line" in {
    assert(winner(parseRound("8C TS KC 9H 4S 7D 2S 5D 3S AC")) match {
      case Player2(_) => true
      case Player1(_) => false
    })
  }

  it should "return the correct answer" in {
    assert(Problem54.solution() == 376)
  }
}
