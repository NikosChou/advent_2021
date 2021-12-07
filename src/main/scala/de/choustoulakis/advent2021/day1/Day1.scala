package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day1.Day1.{Input, Normalizer, ZERO}


trait Day1 extends Puzzle[Input, Int] {
  val day = 1

  override def solve(input: Input): Int = {
    val (depths, normalizer) = input

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
  type Input = (List[Int], Normalizer)
  type Normalizer = 0 | 3

  val ZERO: Normalizer = 0
}
