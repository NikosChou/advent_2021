package de.choustoulakis.advent2021.day4

trait Day4 {
  def hitFisrt(input: String): Int = Game(input).play

  def hitLast(input: String): Int = Game(input).playLast
}

