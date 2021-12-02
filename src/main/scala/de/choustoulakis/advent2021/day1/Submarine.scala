package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.day1.Submarine.{Norm, ZERO}

class Submarine {
  def countTimesDepthChanged(depths: List[Int], normalizer: Norm = ZERO): Int = countTimesDepthChanged {
    if (normalizer == ZERO)
      depths
    else
      depths.zipWithIndex.map((e, i) => depths.drop(i).take(normalizer).sum)
  }

  private def countTimesDepthChanged(depths: List[Int]): Int = depths.zip(depths.tail).filter { case (a, b) => a < b }.size
}

object Submarine {
  type Norm = 0 | 3
  val ZERO: Norm = 0
}
