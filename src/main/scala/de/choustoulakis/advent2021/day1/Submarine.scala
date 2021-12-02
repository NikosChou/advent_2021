package de.choustoulakis.advent2021.day1

import de.choustoulakis.advent2021.day1.Submarine.{Coordinate, Norm, START, ZERO}
import de.choustoulakis.advent2021.day2.{Command, Down, Forward, Up}

class Submarine {

  def countTimesDepthChanged(depths: List[Int], normalizer: Norm = ZERO): Int = countTimesDepthChanged {
    if (normalizer == ZERO)
      depths
    else
      depths.zipWithIndex.map((e, i) => depths.drop(i).take(normalizer).sum)
  }

  def calculatePosition(commands: List[Command]): Coordinate = commands.foldLeft(START)(_ move _)

  private def countTimesDepthChanged(depths: List[Int]): Int = depths.zip(depths.tail).filter { case (a, b) => a < b }.size
}

object Submarine {
  type Norm = 0 | 3

  case class Coordinate(x: Int, y: Int, depth: Int = 0) {
    val result = x * y
    val resultWithAim = x * depth

    def +(coordinate: Coordinate): Coordinate = {
      val newX = coordinate.x + x
      val newY = coordinate.y + y
      val newDepth = coordinate.x * y + depth
      Coordinate(newX, newY, newDepth)
    }

    def -(coordinate: Coordinate): Coordinate = {
      copy(y = y - coordinate.y)
    }

    def move(command: Command): Coordinate = command match {
      case Forward(value) => this + Coordinate(value, 0)
      case Down(value) => this + Coordinate(0, value)
      case Up(value) => this - Coordinate(0, value)
    }
  }

  val ZERO: Norm = 0
  val START = Coordinate(0, 0)
}
