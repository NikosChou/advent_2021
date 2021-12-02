package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.day1.Submarine.{Normalizer, START, ZERO}
import de.choustoulakis.advent2021.day2.*

class Submarine {

  def countTimesDepthChanged(depths: List[Int], normalizer: Normalizer = ZERO): Int = {
    def countTimesDepthChanged(depths: List[Int]): Int = depths.zip(depths.tail).filter(_ < _).size

    countTimesDepthChanged {
      normalizer match {
        case ZERO => depths
        case _ => depths.sliding(3).map(_.sum).toList;
      }
    }
  }

  def calculatePosition(commands: List[Command]): Coordinate = commands.foldLeft(START)(_ move _)
}

object Submarine {
  type Normalizer = 0 | 3

  val ZERO: Normalizer = 0
  val START = Coordinate(0, 0)
}
