package day2

object Day2Part1 {
    def solve(implicit x: Int, lines: List[String]): Int = {
        val result = lines
            .map(line => {
                val gameParts = line.split(": ")
                val gameId = gameParts(0).split(" ")(1).toInt
                isGamePossible(
                  gameParts(1),
                  Map(
                    "red" -> 12,
                    "green" -> 13,
                    "blue" -> 14
                  )
                ) match {
                    case true  => gameId
                    case false => 0
                }
            })
            .sum
        result
    }

    def isGamePossible(line: String, maxes: Map[String, Int]): Boolean = {
        line.split("; ")
            .forall(round => {
                round
                    .split(", ")
                    .forall(colorAndValue => {
                        val tokens = colorAndValue.split(" ")
                        val count = tokens(0).toInt
                        val color = tokens(1)
                        (maxes.get(color) getOrElse 0) >= count
                    })
            })

    }
}

object Day2Part2 {
    def solve(lines: List[String]): Int = {
        val result = lines
            .map(line => {
                val gameParts = line.split(": ")
                val gameId = gameParts(0).split(" ")(1).toInt
                maxGameValues(gameParts(1))
            })
            .sum
        result
    }

    def maxGameValues(
        line: String
    ): Int = {
        val x = line
            .split("; ")
            .map(round => {
                val colorMap = round
                    .split(", ")
                    .map(colorAndValue => {
                        val tokens = colorAndValue.split(" ")
                        val count = tokens(0).toInt
                        val color = tokens(1)
                        (color -> count)
                        // (maxes.get(color) getOrElse 0) >= count
                    })
                    .toMap
                Array(
                  colorMap.get("red") getOrElse 0,
                  colorMap.get("green") getOrElse 0,
                  colorMap.get("blue") getOrElse 0
                )
            })
            .transpose
            .map(_.max)
            .product
        x
    }
}
