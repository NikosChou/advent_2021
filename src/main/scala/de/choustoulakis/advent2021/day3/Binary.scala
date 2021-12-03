package de.choustoulakis.advent2021.day3

import de.choustoulakis.advent2021.day3.Binary.Bit

import java.math.BigInteger

case class Binary(bits: List[Bit]) {
  private lazy val value = new BigInteger(bits.foldLeft("")(_ + _), 2).intValue()
  private lazy val stringRepr = Integer.toBinaryString(value)
  lazy val toDecimal: Int = Integer.parseInt(stringRepr, 2);

  override def toString: String = s"Binary(${bits.foldLeft("")(_ + _)})"

  def findCommonBit(): Bit = bits.foldLeft((0, 0)) {
    case (acc, 0) => acc.copy(acc._1 + 1, acc._2)
    case (acc, 1) => acc.copy(acc._1, acc._2 + 1)
  } match {
    case (zeros, ones) if ones >= zeros => 1
    case _ => 0
  }


  def *(other: Binary): Binary = Binary.fromString(Integer.toBinaryString(value * other.value))

  def unary_! : Binary = {
    def not(bit: Bit): Bit = if (bit == 1) 0 else 1

    Binary(bits.map(not).asInstanceOf[List[Bit]])
  }
}

object Binary {
  type Bit = 0 | 1
  
  def fromString(input: String): Binary = Binary(
    input.split("").map(_.toInt).map(_.asInstanceOf[Bit]).toList.asInstanceOf[List[Bit]]
  )
}