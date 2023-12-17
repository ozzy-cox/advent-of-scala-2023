package day3

import scala.util.Try
import scala.util.Success
import scala.util.Failure

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
