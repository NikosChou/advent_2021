package de.choustoulakis.advent2021.day2

import de.choustoulakis.advent2021.day2.Coordinate._

trait Day2 {
  def calculatePosition(commands: List[Command]): Coordinate = commands.foldLeft(START)(_ move _)
}
