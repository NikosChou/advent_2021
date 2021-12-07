package de.choustoulakis.advent2021.day6

import de.choustoulakis.advent2021.SubmarineSpec
import scala.io.Source

class Day6Test extends SubmarineSpec with Day6 {

  "The Fishes" should {
    val input = "3,4,3,1,2"
    "grow" in {
      solve(input, 80) shouldBe 5934
    }
    "grow for ever" in {
      solve(input, 256) shouldBe 26984457539L
    }
  }

    "The Fishes (resource)" should {
      lazy val input = resource.reduce(_ + _)
      "grow" in {
        solve(input, 80) shouldBe 366057
      }
      "grow for ever" in {
        solve(input, 256) shouldBe 1653559299811L
      }
    }


}
