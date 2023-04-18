import scala.util.Random

val suits: List[String] = List("♠", "♥", "♦", "♣")
val values: Range = Range.inclusive(2, 14)

class Card(var suit: String, var value: Int) {
  override def toString = value + " of " + suit
}

class Player(var name: String, var hand: List[Card]) {
  override def toString = name + " has " + hand
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
    scala.util.Random.shuffle(cards)  
  }

  def deal(players: List[Player], nCards: Int): List[Player] = {
    var newPlayers: List[Player] = List()
    for (p <- players) {
      val (newHand, newDeck) = cards.splitAt(nCards)
      newPlayers = Player(p.name, newHand) :: newPlayers
    }
    newPlayers
  }

  override def toString = cards.toString
}


@main def main = {
  val deck = Deck()
}