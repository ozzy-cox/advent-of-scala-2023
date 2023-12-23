package day4
import scala.math.pow

object Day4Part1 {
    def solve(input: String): Int = {
        input
            .split(
              "\n"
            )
            .map(line =>
                val cardParts = line.split(":")
                val numberParts = cardParts(1).split("\\|")
                val winningSet =
                    numberParts(0)
                        .split("\\s+")
                        .filter(_.nonEmpty)
                        .map(_.toInt)
                        .toSet
                val hand = numberParts(1)
                    .split("\\s+")
                    .filter(_.nonEmpty)
                    .map(_.toInt)
                    .toSet
                val matches: Int = hand.intersect(winningSet).size
                if matches == 0 then 0
                else pow(2, if matches > 0 then matches - 1 else 0).toInt
            )
            .sum
    }

}

object Day4Part2 {

    def solve(input: String): Int = {
        val lines = input
            .split(
              "\n"
            )

        val cards = lines
            .map(line => {
                val cardParts = line.split(":")
                val numberParts = cardParts(1).split("\\|")
                (
                  cardParts(0).split("\\s+").filter(_.nonEmpty)(1).toInt,
                  line
                )
            })
            .toMap
        val cache = collection.mutable.Map.empty[Int, Int]
        solveHelper(cards, cache, lines(5))
        lines.map(solveHelper(cards, cache, _)).sum
    }

    def solveHelper(
        cards: Map[Int, String],
        cache: collection.mutable.Map[Int, Int],
        line: String
    ): Int = {
        val cardParts = line.split(":")
        val cardNumber = cardParts(0).split("\\s+").filter(_.nonEmpty)(1).toInt
        cache.get(cardNumber) match
            case None => {
                val numberParts = cardParts(1).split("\\|")
                val winningSet =
                    numberParts(0)
                        .split("\\s+")
                        .filter(_.nonEmpty)
                        .map(_.toInt)
                        .toSet
                val hand = numberParts(1)
                    .split("\\s+")
                    .filter(_.nonEmpty)
                    .map(_.toInt)
                    .toSet
                val matches: Int = hand.intersect(winningSet).size
                val cardValue =
                    1 + Range(cardNumber + 1, cardNumber + 1 + matches)
                        .map(match_count => {
                            cards.get(match_count) match
                                case None => 0
                                case Some(value) =>
                                    solveHelper(cards, cache, value)

                        })
                        .sum
                cache.update(cardNumber, cardValue)
                cardValue
            }
            case Some(value) => value
    }
}
