package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.day1.Day1.{Normalizer, ZERO}


trait Day1 {
  def countTimesDepthChanged(depths: List[Int], normalizer: Normalizer = ZERO): Int = {
    def countTimesDepthChanged(depths: List[Int]): Int = depths.zip(depths.tail).filter(_ < _).size

    countTimesDepthChanged {
      normalizer match {
        case ZERO => depths
        case _ => depths.sliding(3).map(_.sum).toList;
      }
    }
  }
}

object Day1 {
  type Normalizer = 0 | 3

  val ZERO: Normalizer = 0
}
