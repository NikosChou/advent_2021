package de.choustoulakis.advent2021.day2

import de.choustoulakis.advent2021.SubmarineSpec
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Day2Test extends SubmarineSpec with Day2 {

  "The submarine pilot" should {
    val input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2".split("\\n")
      .map(Command(_))
      .toList
    "calculate position" in {
      solve(input).result shouldBe 150
    }

    "calculate position new process" in {
      solve(input).resultWithAim shouldBe 900
    }
  }

  "The submarine sonar (Resource)" should {
    val input = resource.map(Command(_)).toList
    "calculate position " in {
      solve(input).result shouldBe 1488669
    }

    "calculate position new process" in {
      solve(input).resultWithAim shouldBe 1176514794
    }
  }
}
