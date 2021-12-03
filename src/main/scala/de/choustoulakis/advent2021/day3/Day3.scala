package de.choustoulakis.advent2021.day3

import de.choustoulakis.advent2021.day3.Day3.*
import de.choustoulakis.advent2021.day3.Binary.*

import java.math.BigInteger

trait Day3 {

  def calculatePowerConsumption(input: List[Binary]): PowerConsumption = {
    def findGammaRate(input: List[Binary]): Binary = {
      Binary(
        (0 until input.head.bits.length)
          .map(i => input.map(_.bits(i)))
          .map(Binary(_))
          .map(_.findCommonBit)
          .toList.asInstanceOf[List[Bit]]
      )
    }

    PowerConsumption(findGammaRate(input))
  }

  def calculateOxygeRate(input: List[Binary]): LifeSupport = {
    def findLastBinary(bitMapper: Binary => Bit, rest: List[Binary] = input, position: Int = 0): Binary = {
      if (rest.tail.isEmpty) {
        return rest.head
      }

      val bit = bitMapper(Binary(rest.map(_.bits(position))))
      val filtered = rest.filter(bin => bin.bits(position) == bit)
      findLastBinary(bitMapper, filtered, position + 1)
    }

    val findOxygen = findLastBinary(_.findCommonBit)
    val findCo2 = findLastBinary(b => b.findCommonBit.not)
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
