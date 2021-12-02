package de.choustoulakis.advent2021.day2

import de.choustoulakis.advent2021.day1.Submarine
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class SubmarineTest extends AnyWordSpec with Matchers {

  "The submarine pilot" should {
    val input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2".split("\\n")
      .map(Command(_))
      .toList
    "calculate position" in {
      val submarine = Submarine()
      submarine.calculatePosition(input).result shouldBe 150
    }

    "calculate position new process" in {
      val submarine = Submarine()
      submarine.calculatePosition(input).resultWithAim shouldBe 900
    }
  }

  "The submarine sonar (Resource)" should {
    val input = Source.fromResource("day2/input.txt").getLines().map(Command(_)).toList
    "calculate position " in {
      val sonarSweep = Submarine()
      sonarSweep.calculatePosition(input).result shouldBe 1488669
    }

    "calculate position new process" in {
      val submarine = Submarine()
      submarine.calculatePosition(input).resultWithAim shouldBe 1176514794
    }
  }
}
