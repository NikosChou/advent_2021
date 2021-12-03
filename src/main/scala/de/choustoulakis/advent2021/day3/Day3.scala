package de.choustoulakis.advent2021.day3

import de.choustoulakis.advent2021.day3.Day3.*
import de.choustoulakis.advent2021.day3.Binary.*

import java.math.BigInteger

trait Day3 {

  def calculatePowerConsumption(input: List[Binary]): PowerConsumption = {
    def findGammaRate(input: List[Binary]): Binary = {
      Binary(
        (0 until input.head.bits.length)
          .map(i => Binary(input.map(b => b.bits(i))))
          .map(_.findCommonBit())
          .toList.asInstanceOf[List[Bit]]
      )
    }

    PowerConsumption(findGammaRate(input))
  }

  def calculateOxygeRate(input: List[Binary]): LifeSupport = {
    def findLastBinary(bitsMapper: PowerConsumption => Binary, rest: List[Binary] = input, position: Int = 0): Binary = {
      if (rest.tail.isEmpty) {
        return rest.head
      }

      val bits: List[Bit] = bitsMapper(calculatePowerConsumption(rest)).bits
      val filtered = rest.filter(bin => bin.bits(position) == bits(position))
      findLastBinary(bitsMapper, filtered, position + 1)
    }

    val findOxygen = findLastBinary(_.gamma)
    val findCo2 = findLastBinary(_.epsilon)
    LifeSupport(findOxygen, findCo2)
  }
}

object Day3 {

  case class LifeSupport(oxygenRate: Binary, co2Rate: Binary) {
    val rate = (oxygenRate * co2Rate).toDecimal
  }

  case class PowerConsumption(gamma: Binary) {
    val epsilon = !gamma
    val calculatePowerConsumption = gamma * epsilon
  }
}
