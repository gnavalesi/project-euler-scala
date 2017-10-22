package euler.problems

import euler.utils.Problem

import scala.io.Source

/**
  * Problem 54
  *
  * In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
  *
  * High Card: Highest value card.
  * One Pair: Two cards of the same value.
  * Two Pairs: Two different pairs.
  * Three of a Kind: Three cards of the same value.
  * Straight: All cards are consecutive values.
  * Flush: All cards of the same suit.
  * Full House: Three of a kind and a pair.
  * Four of a Kind: Four cards of the same value.
  * Straight Flush: All cards are consecutive values of same suit.
  * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
  *
  * The cards are valued in the order:
  * 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
  *
  * If two players have the same ranked hands then the rank made up of the highest value wins; for example,
  * a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example,
  * both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
  * if the highest cards tie then the next highest cards are compared, and so on.
  *
  * Consider the following five hands dealt to two players:
  *
  * Hand	 	Player 1	 	        Player 2	 	        Winner
  * 1       5H 5C 6S 7S KD      2C 3S 8S 8D TD      Player 2
  * Pair of Fives       Pair of Eights
  *
  * 2	 	    5D 8C 9S JS AC      2C 5C 7D 8S QH      Player 1
  * Highest card Ace    Highest card Queen
  *
  * 3   	 	2D 9C AS AH AC      3D 6D 7D TD QD      Player 2
  * Three Aces          Flush with Diamonds
  *
  * 4	 	    4D 6S 9H QH QC      3D 6D 7H QD QS      Player 1
  * Pair of Queens      Pair of Queens
  * Highest card Nine   Highest card Seven
  *
  * 5	 	    2H 2D 4C 4D 4S      3C 3D 3S 9S 9D      Player 1
  * Full House          Full House
  * With Three Fours    With Three Threes
  *
  * The file, poker.txt, contains one-thousand random hands dealt to two players.
  * Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards
  * and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters
  * or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.
  *
  * How many hands does Player 1 win?
  *
  **/
object Problem54 extends Problem with App {

  trait Suit

  case object Diamonds extends Suit

  case object Hearts extends Suit

  case object Clubs extends Suit

  case object Spaces extends Suit

  private def parseSuit(char: Char): Suit = char match {
    case 'D' => Diamonds
    case 'H' => Hearts
    case 'C' => Clubs
    case 'S' => Spaces
  }

  trait Card extends Ordered[Card] {
    val value: Int
    val suit: Suit

    override def compare(that: Card): Int = value.compare(that.value)
  }

  object Card {
    def apply(theValue: Int, theSuit: Suit): Card = {
      new Card {
        override val suit: Suit = theSuit
        override val value: Int = theValue
      }
    }
  }

  case class Jack(suit: Suit) extends Card {
    override val value: Int = 11
  }

  case class Queen(suit: Suit) extends Card {
    override val value: Int = 12
  }

  case class King(suit: Suit) extends Card {
    override val value: Int = 13
  }

  case class Ace(suit: Suit) extends Card {
    override val value: Int = 14
  }

  private def parseCard(string: String): Card = {
    string.charAt(0) match {
      case 'T' => Card(10, parseSuit(string.charAt(1)))
      case 'J' => Jack(parseSuit(string.charAt(1)))
      case 'Q' => Queen(parseSuit(string.charAt(1)))
      case 'K' => King(parseSuit(string.charAt(1)))
      case 'A' => Ace(parseSuit(string.charAt(1)))
      case c: Char => Card(c.toString.toInt, parseSuit(string.charAt(1)))
    }
  }

  trait Hand extends Ordered[Hand] {
    val cards: Seq[Card]
    val value: Int
    type T <: Hand

    override def compare(that: Hand): Int = value.compare(that.value) match {
      case 0 => this.compareCombination(that.asInstanceOf[T]) match {
        case 0 => compareCards(that)
        case r => r
      }
      case r => r
    }

    def compareCombination(that: T): Int

    def compareCards(that: Hand): Int = cards.sorted.reverse.zip(that.cards.sorted.reverse)
      .map(cs => cs._1.compare(cs._2))
      .find(_ != 0)
      .getOrElse(0)
  }

  case class RoyalFlush(cards: Seq[Card]) extends Hand {
    val value: Int = 23
    type T = StraightFlush

    override def compareCombination(that: StraightFlush): Int = 0
  }

  case class StraightFlush(cards: Seq[Card]) extends Hand {
    val value: Int = 22
    type T = StraightFlush

    override def compareCombination(that: StraightFlush): Int = 0
  }

  case class FourKind(cards: Seq[Card], four: (Card, Card, Card, Card)) extends Hand {
    val value: Int = 21
    type T = FourKind
    val fourValue: Int = four._1.value

    override def compareCombination(that: FourKind): Int = fourValue.compare(that.fourValue)
  }

  case class FullHouse(cards: Seq[Card], pair: Pair, three: ThreeKind) extends Hand {
    val value: Int = 20
    type T = FullHouse

    def compareCombination(that: FullHouse): Int = three.threeValue.compare(that.three.threeValue) match {
      case 0 => pair.pairValue.compare(that.pair.pairValue)
      case r => r
    }
  }

  case class Flush(cards: Seq[Card]) extends Hand {
    val value: Int = 19
    type T = Flush

    override def compareCombination(that: Flush): Int = 0
  }

  case class Straight(cards: Seq[Card]) extends Hand {
    val value: Int = 18
    type T = Straight

    override def compareCombination(that: Straight): Int = 0
  }

  case class ThreeKind(cards: Seq[Card], three: (Card, Card, Card)) extends Hand {
    val value: Int = 17
    type T = ThreeKind
    val threeValue: Int = three._1.value

    override def compareCombination(that: ThreeKind): Int = threeValue.compare(that.threeValue)
  }

  case class DoublePair(cards: Seq[Card], pair1: Pair, pair2: Pair) extends Hand {
    val value: Int = 16
    type T = DoublePair
    val maxPairValue: Int = pair1.pairValue.max(pair2.pairValue)
    val minPairValue: Int = pair1.pairValue.min(pair2.pairValue)

    def compareCombination(that: DoublePair): Int = maxPairValue.compare(that.maxPairValue) match {
      case 0 => minPairValue.compare(that.minPairValue)
      case r => r
    }
  }

  case class Pair(cards: Seq[Card], pair: (Card, Card)) extends Hand {
    val value: Int = 15
    val pairValue: Int = pair._1.value
    type T = Pair

    def compareCombination(that: Pair): Int = pairValue.compare(that.pairValue)
  }

  case class HighCard(cards: Seq[Card]) extends Hand {
    val value: Int = cards.max.value
    type T = HighCard

    def compareCombination(that: HighCard): Int = 0
  }

  object Hand {
    private def pair(combinations: Iterable[Seq[Card]]): (Card, Card) = {
      val pair = combinations.find(_.size == 2).get
      (pair.head, pair(1))
    }

    private def three(combinations: Iterable[Seq[Card]]): (Card, Card, Card) = {
      val four = combinations.find(_.size == 3).get
      (four.head, four(1), four(2))
    }

    def apply(cards: Seq[Card]): Hand = cards match {
      case _ if isFlush(cards) => cards match {
        case _ if cards.min.value == 10 => RoyalFlush(cards)
        case _ if isStraight(cards) => StraightFlush(cards)
        case _ => Flush(cards)
      }
      case _ if isStraight(cards) => Straight(cards)
      case _ =>
        val cardsGrouped = cards.groupBy(_.value).values

        cardsGrouped.size match {
          case 5 => HighCard(cards)
          case 4 => Pair(cards, pair(cardsGrouped))
          case 3 =>
            val three = cardsGrouped.find(_.size == 3)
            if (three.isDefined) {
              ThreeKind(cards, (three.get.head, three.get.apply(1), three.get.apply(2)))
            } else {
              val pairs = cardsGrouped.filter(_.size == 2)
              DoublePair(cards, Pair(cards, (pairs.head.head, pairs.head.apply(1))),
                Pair(cards, (pairs.tail.head.head, pairs.tail.head.apply(1))))
            }
          case 2 =>
            val four = cardsGrouped.find(_.size == 4)
            if (four.isDefined) {
              FourKind(cards, (four.get.head, four.get.apply(1), four.get.apply(2), four.get.apply(3)))
            } else {
              FullHouse(cards, Pair(cards, pair(cardsGrouped)), ThreeKind(cards, three(cardsGrouped)))
            }

        }
    }

    private def isFlush(cards: Seq[Card]): Boolean = cards.map(c => c.suit).distinct.length == 1

    private def isStraight(cards: Seq[Card]): Boolean =
      cards.map(_.value).distinct.size == cards.size &&
        cards.max.value - cards.min.value == 4
  }

  def parseHand(string: String): Hand = Hand(string.trim.split(" ").map(c => parseCard(c.trim)))

  trait Player extends Ordered[Player] {
    val hand: Hand

    override def compare(that: Player): Int = hand.compare(that.hand)
  }

  case class Player1(hand: Hand) extends Player

  case class Player2(hand: Hand) extends Player

  case class Round(player1: Player1, player2: Player2)

  def parseRound(string: String): Round = Round(Player1(parseHand(string.substring(0, 14))),
    Player2(parseHand(string.substring(15))))

  def winner(round: Round): Player = if (round.player1 > round.player2) round.player1 else round.player2

  override def solution(): Any = Source.fromResource("problem_54.txt").mkString.split("\n")
    .map(parseRound)
    .map(winner)
    .count {
      case Player1(_) => true
      case _ => false
    }
}
