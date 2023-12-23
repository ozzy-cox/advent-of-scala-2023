package day3

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.matching.Regex.Match

object Day3Part1 {
    val numberPattern = "[0-9]+".r
    def solve(lines: Array[String]): Int = {
        val grid = lines.map(_.toCharArray())
        lines.zipWithIndex
            .map((value, idx) =>
                numberPattern
                    .findAllMatchIn(value)
                    .filter(m => {
                        val values = List(1, 0, -1)
                        val offsets =
                            values.flatMap(y => values.map(x => (y, x)))
                        val r = Range(m.start, m.end)
                            .flatMap(y =>
                                offsets.map(offset =>
                                    (y + offset(0), idx + offset(1))
                                )
                            )
                            .filterNot(item =>
                                Range(m.start, m.end)
                                    .map(_y => (_y, idx))
                                    .exists(_ == item)
                            )
                            .toSet
                            .exists((x, y) =>
                                Try(
                                  grid(y)(x)
                                ) match {
                                    case Success(k) =>
                                        (List('.') ::: Range(0, 10)
                                            .map(_.toString())
                                            .toList)
                                            .forall(s => s != k)
                                    case Failure(exception) => false
                                }
                            )
                        r
                    })
                    .map(_.matched.toInt)
                    .toList
                    .sum
            )
            .sum
    }
}

object Day3Part2 {
    val numberPattern = """(\d+)|[^.\d]""".r
    def solve(input: String): Int = {
        val lines = input.split("\n")
        val grid = lines.map(_.toCharArray())
        val numbers = lines.zipWithIndex
            .map((value, idx) =>
                (
                  idx,
                  numberPattern
                      .findAllMatchIn(value)
                      .map(m => (m.start, m.end, m.matched))
                      .toList
                )
            )
            .toMap

        lines.zipWithIndex
            .flatMap((line, idx) =>
                """[^.\d]""".r
                    .findAllMatchIn(line)
                    .map(m => {
                        val values = List(1, 0, -1)
                        val offsets =
                            values.flatMap(y => values.map(x => (y, x)))
                        val adjacentNumbers = {
                            offsets
                                .map(offset =>
                                    (m.start + offset(0), idx + offset(1))
                                )
                                .filter((y, x) => grid(x)(y).isDigit)
                                .map((y, x) => {
                                    val k = numbers
                                        .get(x)
                                        .flatMap(num =>
                                            num.find(number =>
                                                Range(number(0), number(1))
                                                    .exists(rangeVal =>
                                                        rangeVal == y
                                                    )
                                            )
                                        )
                                    k
                                })
                                .toSet
                        }
                        adjacentNumbers
                    })
                    .filter(
                      _.size == 2
                    )
                    .map(item => {
                        item.toList.flatMap(o => o.map(v => v(2).toInt))
                    })
                    .map((s) => {
                        s.product
                    })
            )
            .sum
    }
}
