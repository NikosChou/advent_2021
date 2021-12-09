package de.choustoulakis.advent2021.day9

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day9Test extends SubmarineSpec with Day9 {
  "The Day9" should {
    val input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 15
      }

    "solve part 2" in {
      result.part2 shouldBe 1134
    }
  }

  "The Day9 Resource" should {
    val input = resource.mkString("\n")
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 498
      }

    "solve part 2" in {
      result.part2 shouldBe 1071000
    }
  }

}
