package de.choustoulakis.advent2021.day4

import de.choustoulakis.advent2021.day4.Board.Input

case class Game(input: Input, boards: Boards) {
  lazy val play: Int = {
    input.foldLeft(boards) {
      case (rest@Boards(_ :: Nil), _) if rest.maybeWinner.isDefined => rest
      case (rest, in) if rest.maybeWinner.isEmpty => rest.hit(in)
      case (rest, _) => Boards(rest.boards.filter(_.isFinished))
    }.calculateRest
  }

  lazy val playLast: Int = {
    input.foldLeft(boards) {
      case (rest@Boards(_ :: Nil), _) if (rest.maybeWinner.isDefined) => rest
      case (rest, in) if (rest.maybeWinners.nonEmpty) => rest.notWinners.hit(in)
      case (rest, in) => rest.hit(in)
    }.calculateRest
  }
}

object Game {
  def apply(inputStr: String): Game = {
    val strings = inputStr.split("\\n\\n")
    val input: Input = strings.head.split(",").toList
    val boards: Boards = Boards(strings.tail)
    Game(input, boards)
  }
}
