package de.choustoulakis.advent2021.day20

import de.choustoulakis.advent2021.SubmarineSpec

import scala.io.Source

class Day20Test extends SubmarineSpec with Day20 {

  "The Pre puzzle code" should {
    lazy val resource = Source.fromResource(s"day20/test-input.txt").getLines().toList
    val result = solve(resource)
    "solve part1" in {
      result.part1 shouldBe 35
    }
    "solve part2" in {
      result.part2 shouldBe 3351
    }
  }

  "The day 20 Resource" should {
    val result = solve(resource)
    "solve part 1" in {
      result.part1 shouldBe 5489
    }
    "solve part 2" in {
      result.part2 shouldBe 19066
    }
  }

}
