package de.choustoulakis.advent2021.day3

sealed trait Bit(val b: Int) {
  def not: Bit

  override def toString: String = b.toString
}

object Bit {
  case object ZERO extends Bit(0) {
    val not = ONE
  }

  case object ONE extends Bit(1) {
    val not = ZERO
  }

  def apply(b: Int): Bit = b match {
    case 0 => ZERO
    case _ => ONE
  }
}
