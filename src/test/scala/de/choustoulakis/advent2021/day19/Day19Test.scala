package de.choustoulakis.advent2021.day19

import de.choustoulakis.advent2021.SubmarineSpec
import de.choustoulakis.advent2021.day19.Day19.*

import scala.io.Source

class Day19Test extends SubmarineSpec with Day19 {

  "The Pre puzzle code" should {
    lazy val resource = Source.fromResource(s"day19/test-input.txt").getLines().toList
    val result = solve(resource)
    "solve part1" in {
      result.part1 shouldBe 79
    }
    "solve part2" in {
      result.part2 shouldBe 3621
    }
  }

  "The day 17 Resource" should {
    val result = solve(resource)
    "solve part 1" in {
      result.part1 shouldBe 318
    }
    "solve part 2" in {
      result.part2 shouldBe 12166
    }
  }

}
