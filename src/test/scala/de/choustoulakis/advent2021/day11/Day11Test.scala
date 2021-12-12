package de.choustoulakis.advent2021.day11

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day11Test extends SubmarineSpec with Day11 {
  "The Day11" should {
    val input = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 1656
    }

    "solve part 2" in  {
      result.part2 shouldBe 195
    }
  }

  "The Day11 Resource" should {
    val input = resource.mkString("\n")
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 1588
    }

    "solve part 2" in {
      result.part2 shouldBe 517
    }
  }

}
