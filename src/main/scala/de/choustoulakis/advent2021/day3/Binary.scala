package de.choustoulakis.advent2021.day3

import de.choustoulakis.advent2021.day3.Bit.{ONE, ZERO}

import java.math.BigInteger

case class Binary(bits: List[Bit]) {
  private lazy val value = new BigInteger(bits.foldLeft("")(_ + _), 2).intValue()
  private lazy val stringRepr = bits.foldLeft("")(_ + _)
  lazy val toDecimal: Int = Integer.parseInt(stringRepr, 2)

  override def toString: String = s"Binary($stringRepr)"

  lazy val findCommonBit: Bit = bits.foldLeft((0, 0)) {
    case (acc, ZERO) => acc.copy(acc._1 + 1, acc._2)
    case (acc, ONE) => acc.copy(acc._1, acc._2 + 1)
  } match {
    case (zeros, ones) if ones >= zeros => ONE
    case _ => ZERO
  }


  def *(other: Binary): Binary = Binary.fromString(Integer.toBinaryString(value * other.value))

  def unary_! : Binary = {
    Binary(bits.map(_.not).asInstanceOf[List[Bit]])
  }
}

object Binary {
  def fromString(input: String): Binary = Binary(
    input.split("")
      .map(_.toInt)
      .map(Bit(_))
      .toList
  )
}