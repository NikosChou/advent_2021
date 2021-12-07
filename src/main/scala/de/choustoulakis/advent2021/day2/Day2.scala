package de.choustoulakis.advent2021.day2

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day2.Coordinate.*

trait Day2 extends Puzzle[List[Command], Coordinate] {
  val day = 2
  override def solve(commands: List[Command]): Coordinate = commands.foldLeft(START)(_ move _)
}
