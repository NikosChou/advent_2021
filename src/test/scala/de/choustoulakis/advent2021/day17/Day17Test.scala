package de.choustoulakis.advent2021.day17

import de.choustoulakis.advent2021.SubmarineSpec

class Day17Test extends SubmarineSpec with Day17 {

  "The day17" should {
    val result = solve("target area: x=20..30, y=-10..-5")
    "solve part1" in {
      result.part1 shouldBe 45
    }
    "solve part2" in {
      result.part2 shouldBe 112
    }
  }

  "The day 17 Resource" should {
    val result = solve(resource.head)
    "solve part 1" in {
      result.part1 shouldBe 14535
    }
    "solve part 2" in {
      result.part2 shouldBe 2270
    }
  }

}
