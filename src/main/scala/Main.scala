import scala.io.Source
import day1.Day1Part1
import day1.Day1Part2
import day2.Day2Part1
import day2.Day2Part2
import day3.Day3Part1
import day3.Day3Part2
import day4.Day4Part1
import day4.Day4Part2
import day5.Day5Part1
import day5.Day5Part2

object Main extends App {
    val input = Source.fromFile("src/main/scala/day5/input.txt").mkString
    println(Day5Part2.solve(input))

}
