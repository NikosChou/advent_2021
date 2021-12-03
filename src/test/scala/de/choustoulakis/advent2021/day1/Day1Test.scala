package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.SubmarineSpec
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Day1Test extends SubmarineSpec with Day1 {

  "The submarine sonar" should {
    val input = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263".split("\\n").map(_.toInt).toList
    "count the number of times a depth measurement increases " in {
      countTimesDepthChanged(input) shouldBe 7
    }

    "count the number of times a depth measurement increases noise reduced" in {
      countTimesDepthChanged(input, 3) shouldBe 5
    }
  }

  "The submarine sonar (Resource)" should {
    val input = Source.fromResource("day1/input.txt").getLines().map(_.toInt).toList
    "count the number of times a depth measurement increases " in {
      countTimesDepthChanged(input) shouldBe 1451
    }

    "count the number of times a depth measurement increases noise reduced" in {
      countTimesDepthChanged(input, 3) shouldBe 1395
    }
  }
}
