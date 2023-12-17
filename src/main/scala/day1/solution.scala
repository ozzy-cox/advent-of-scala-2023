package day1

import scala.util.matching.Regex

object Day1Part1 {
    def solveDay1(lines: List[String]): Int = {
        lines.tail match
            case head :: next => solveLine(lines.head) + solveDay1(lines.tail)
            case Nil          => solveLine(lines.head)
    }

    def solveLine(line: String): Int = {
        val first = line.find(_.isDigit).map(_.asDigit * 10) getOrElse 0
        val last = line.findLast(_.isDigit).map(_.asDigit) getOrElse 0
        first + last
    }
}

object Day1Part2 {
    val digitRegex =
        """(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))""".r
    val digitValueMap: Map[String, Int] = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    )
    def solveDay1(lines: List[String]): Int = {
        lines.tail match
            case head :: next =>
                solveLine(lines.head, lines.size) + solveDay1(lines.tail)
            case Nil => solveLine(lines.head, lines.size)
    }

    def solveLine(line: String, len: Int): Int = {
        val first = digitRegex
            .findFirstMatchIn(line)
            .map { case digitRegex(x) => s"$x" }
            .flatMap(_ match {
                case digit if digitValueMap.contains(digit) =>
                    digitValueMap.get(digit)
                case digit if digit != "" =>
                    Some(digit.toInt)
                case _ => {
                    println("err")
                    None
                }
            })
            .map(d => d * 10)

        val last = (digitRegex
            .findAllMatchIn(line)
            .map { case digitRegex(x) => s"$x" }
            .toList
            .last match {
            case digit if digitValueMap.contains(digit) =>
                digitValueMap.get(digit)
            case digit if digit != "" =>
                Some(digit.toInt)
            case _ => {
                None
            }

        })
        first.flatMap(_first => last.map(_ + _first)) getOrElse 0
    }
}
