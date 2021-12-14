package de.choustoulakis.advent2021.day12

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day12Test extends SubmarineSpec with Day12 {
  "The Day12" should {
    val input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 10
    }

    "solve part 2" in {
      result.part2 shouldBe 36
    }
  }

  "The Day12 Resource" should {
    val input = resource.mkString("\n")
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 3421
    }

    "solve part 2" in {
      result.part2 shouldBe 84870
    }
  }

}
