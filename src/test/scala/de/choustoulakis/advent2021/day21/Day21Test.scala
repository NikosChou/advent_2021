package de.choustoulakis.advent2021.day21

import de.choustoulakis.advent2021.SubmarineSpec

import scala.io.Source

class Day21Test extends SubmarineSpec with Day21 {

  "The Pre puzzle code" should {
    val input = "Player 1 starting position: 4\nPlayer 2 starting position: 8"
    val result = solve(input)
    "solve part1" in {
      result.part1 shouldBe 739785
    }
    "solve part2" in {
      result.part2 shouldBe 444356092776315L
    }
  }

  "The day 21 Resource" should {
    val result = solve("Player 1 starting position: 5\nPlayer 2 starting position: 9")
    "solve part 1" in {
      result.part1 shouldBe 989352
    }
    "solve part 2" in {
      result.part2 shouldBe 430229563871565L
    }
  }

}
