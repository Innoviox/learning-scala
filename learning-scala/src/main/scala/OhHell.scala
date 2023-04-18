import scala.util.Random

val suits: List[String] = List("♠", "♥", "♦", "♣")
val values: Range = Range.inclusive(2, 14)

class Card(var suit: String, var value: Int) {
  override def toString = value + " of " + suit
}

class Player(var name: String, var hand: List[Card]) {
  override def toString = name + " has " + hand

  def playRound(leadSuit: String): Card = {
    val leadCards = hand.filter(_.suit == leadSuit)
    if (leadCards.size > 0) {
      val card = leadCards.head
      hand = hand.filterNot(_ == card)
      card
    } else {
      val card = hand.head
      hand = hand.filterNot(_ == card)
      card
    }
  }
}

class Deck(var cards: List[Card] = List()) {
  def makeDeck(): Unit = {
    cards = List()
    for (s <- suits) {
      for (v <- values) {
        val card = Card(s, v)
        cards = card :: cards
      }
    }
    cards = scala.util.Random.shuffle(cards)  
  }

  def deal(players: List[Player], nCards: Int): List[Player] = {
    var newPlayers: List[Player] = List()
    for (p <- players) {
      val (newHand, newDeck) = cards.splitAt(nCards)
      newPlayers = Player(p.name, newHand) :: newPlayers
      cards = newDeck
    }
    newPlayers
  }

  def trumpSuit(): String = {
    cards.head.suit
  }

  override def toString = cards.toString
}

class Game(var players: List[Player], var deck: Deck, var round: Int) {
  def play(): Unit = {
    deck.makeDeck()
    players = deck.deal(players, round)

    println("Round " + round)
    println("Trump Suit is " + deck.trumpSuit());

    var nextLeader = players.head
    for (i <- 1 to round) {
      println("Trick " + i)
      nextLeader = playRound(nextLeader)
    }
  }

  def playRound(leadPlayer: Player): Player = {
    println("Lead Player is " + leadPlayer.name)
    println("Lead Player's hand is " + leadPlayer.hand)

    val index = players.indexOf(leadPlayer)
    var currIndex = index

    var leadSuit: String = null;

    var bestCard: Card = null;
    var bestPlayer: Player = null;

    while (currIndex != index + players.size) {
      if (currIndex == index) {
        val card = leadPlayer.playRound("");
        println("Lead Player leads " + card);

        leadSuit = card.suit
        bestCard = card
        bestPlayer = leadPlayer
      } else {
        val currPlayer = players(currIndex % players.size)
        println("Next player is " + currPlayer.name)
        println(currPlayer.name + "'s hand is " + currPlayer.hand)

        val card = players(currIndex % players.size).playRound(leadSuit)
        println(currPlayer.name + " plays " + card)

        if (card.suit == deck.trumpSuit()) {
          if (bestCard.suit != deck.trumpSuit()) {
            bestCard = card
            bestPlayer = currPlayer
          } else {
            if (card.value > bestCard.value) {
              bestCard = card
              bestPlayer = currPlayer
            }
          }
        } else if (card.suit == leadSuit && bestCard.suit != deck.trumpSuit()) {
          if (card.value > bestCard.value) {
            bestCard = card
            bestPlayer = currPlayer
          }
        }
      }

      currIndex += 1
    }

    println(bestPlayer.name + " wins the round with " + bestCard)

    bestPlayer
  }
}


@main def main = {
  val deck = Deck()

  val players = List(
    Player("Alice", List()),
    Player("Bob", List()),
    Player("Carol", List()),
    Player("Dave", List())
  )

  new Game(players, deck, 2).play()
}