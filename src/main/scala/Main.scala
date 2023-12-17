import scala.io.Source
import day1.Day1Part1
import day1.Day1Part2
import day2.Day2Part1
import day2.Day2Part2
import day3.Day3Part1

object Main extends App {
    val lines =
        Source.fromFile("src/main/scala/day3/input.txt").getLines.toArray

    println(Day3Part1.solve(lines))

}
