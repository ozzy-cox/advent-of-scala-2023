package day5

object Day5Part1 {
    def solve(input: String): Long = {
        val parts = input
            .split(
              "\n\n"
            )
        val seeds = parts.head.split(": ").last.split(" ").map(_.toLong)
        val rest =
            parts.tail.map(converter =>
                val maps =
                    converter
                        .split("\n")
                        .tail
                        .map(values => {
                            val ranges = values.split(" ").map(_.toLong)
                            (
                              (_input: Long) =>
                                  ranges(1) to (ranges(1) + ranges(
                                    2
                                  )) contains _input,
                              (_input: Long) =>
                                  ranges(1) to (ranges(1) + ranges(
                                    2
                                  )) contains _input match {
                                      case true =>
                                          _input + (ranges(0) - ranges(1))
                                      case false => _input
                                  }
                            )
                        })
                (_input: Long) => {
                    val validFuncs = maps
                        .filter((matches, _) => matches(_input))

                    if validFuncs.length > 0 then validFuncs.head(1)(_input)
                    else _input
                }
            )
        seeds
            .map(seed => {
                rest.foldLeft(seed)((acc, curr) => {
                    curr(acc)
                })
            })
            .min
    }
}

object Day5Part2 {
    def solve(input: String): Long = {
        val parts = input
            .split(
              "\n\n"
            )
        val items =
            parts.head
                .split(": ")
                .last
                .split(" ")
                .map(_.toLong)
                .sliding(2, 2)
                .next()
            // .flatMap(items => {

            // })
        val seeds = items(0) to items(0) + items(1)

        // seeds.foreach(a => println(a))
        val rest =
            parts.tail.map(converter =>
                val maps =
                    converter
                        .split("\n")
                        .tail
                        .map(values => {
                            val ranges = values.split(" ").map(_.toLong)
                            (
                              (_input: Long) =>
                                  ranges(1) to (ranges(1) + ranges(
                                    2
                                  )) contains _input,
                              (_input: Long) =>
                                  ranges(1) to (ranges(1) + ranges(
                                    2
                                  )) contains _input match {
                                      case true =>
                                          _input + (ranges(0) - ranges(1))
                                      case false => _input
                                  }
                            )
                        })
                (_input: Long) => {
                    val validFuncs = maps
                        .filter((matches, _) => matches(_input))

                    if validFuncs.length > 0 then validFuncs.head(1)(_input)
                    else _input
                }
            )
        seeds
            .map(seed => {
                rest.foldLeft(seed)((acc, curr) => {
                    curr(acc)
                })
            })
            .min
        // 1
    }

}
