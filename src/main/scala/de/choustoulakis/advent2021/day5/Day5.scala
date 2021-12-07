package de.choustoulakis.advent2021.day5

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day5.Ground.Line

trait Day5 extends Puzzle[List[Line], Int] :
  val day = 5

  override def solve(lines: List[Line]): Int = lines
    .flatMap(_.toPoints)
    .groupBy(p => p)
    .map(_._2.length)
    .filter(_ >= 2)
    .size
