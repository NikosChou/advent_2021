package de.choustoulakis.advent2021.day3

import de.choustoulakis.advent2021.SubmarineSpec
import de.choustoulakis.advent2021.day3.Day3.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Day3Test extends SubmarineSpec with Day3 {

  "The submarine Diagnostics" should {
    val input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split("\\n").map(Binary.fromString).toList
    "calculate PowerConsumption" in {
      val powerConsumption = calculatePowerConsumption(input)
      powerConsumption.gamma shouldBe Binary.fromString("10110")
      powerConsumption.gamma.toDecimal shouldBe 22
      powerConsumption.epsilon shouldBe Binary.fromString("01001")
      powerConsumption.epsilon.toDecimal shouldBe 9
      powerConsumption.calculatePowerConsumption.toDecimal shouldBe 198
    }

    "calculate life support" in {
      val lifeSupport = calculateOxygeRate(input)

      lifeSupport.oxygenRate.toDecimal shouldBe 23
      lifeSupport.co2Rate.toDecimal shouldBe 10
      lifeSupport.rate shouldBe 230
    }
  }

  "Calculate PowerConsumption (Resource input)" should {
    val input = Source.fromResource("day3/input.txt").getLines().map(Binary.fromString(_)).toList
    "calculate PowerConsumption" in {
      val powerConsumption = calculatePowerConsumption(input)
      powerConsumption.gamma shouldBe Binary.fromString("101001001011")
      powerConsumption.gamma.toDecimal shouldBe 2635
      powerConsumption.epsilon shouldBe Binary.fromString("010110110100")
      powerConsumption.epsilon.toDecimal shouldBe 1460
      powerConsumption.calculatePowerConsumption.toDecimal shouldBe 3847100
    }

    "calculate life support" in {
      val lifeSupport = calculateOxygeRate(input)

      lifeSupport.oxygenRate.toDecimal shouldBe 2735
      lifeSupport.co2Rate.toDecimal shouldBe 1501
      lifeSupport.rate shouldBe 4105235
    }
  }
}
