package de.choustoulakis.advent2021.day2

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

object Coordinate {
  val START = Coordinate(0, 0)
}
