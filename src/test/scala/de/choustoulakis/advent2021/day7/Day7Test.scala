package de.choustoulakis.advent2021.day7

import de.choustoulakis.advent2021.SubmarineSpec
import de.choustoulakis.advent2021.day7.Day7.{SumConsecutive, SumAbs}

import scala.io.Source

class Day7Test extends SubmarineSpec with Day7 {
  "The Crabs" should {
    val input = "16,1,2,0,4,2,7,1,2,14".split(",").map(_.toInt).toList
    "cost least fuel 1" in {
      solve(input, SumAbs) shouldBe 37
    }
    "cost least fuel 2" in {
      solve(input, SumConsecutive) shouldBe 168
    }
  }

  "The Crabs resource" should {
    val input = resource.reduce(_ + _).split(",").map(_.toInt).toList
    "cost least fuel 1" in {
      solve(input, SumAbs) shouldBe 345197
    }
    "cost least fuel 2" in {
      solve(input, SumConsecutive) shouldBe 96361606
    }
  }


}
