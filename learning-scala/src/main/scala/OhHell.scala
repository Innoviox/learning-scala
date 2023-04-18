import scala.util.Random

val suits: List[String] = List("♠", "♥", "♦", "♣")
val values: Range = Range.inclusive(2, 14)

class Card(val suit: String, val value: Int) {
  override def toString = value + " of " + suit
}

var deck: List[Card] = List()

@main def main = {
  for (s <- suits) {
    for (v <- values) {
      val card = Card(s, v)
      deck = card :: deck
    }
  }
  scala.util.Random.shuffle(deck)
}