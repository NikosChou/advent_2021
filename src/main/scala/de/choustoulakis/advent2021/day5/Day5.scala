package de.choustoulakis.advent2021.day5

import de.choustoulakis.advent2021.day5.Ground.Line

trait Day5 {
  def solve(lines: List[Line], filter: Line => Boolean = l => l.isHorizontal || l.isVertical): Int = {
    lines
      .filter(filter)
      .flatMap(_.toPoints)
      .groupBy(p => p)
      .map(p => {
        p._2.length
      })
      .filter(_ >= 2)
      .size
  }
}
