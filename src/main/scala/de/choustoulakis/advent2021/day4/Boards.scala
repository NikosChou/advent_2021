package de.choustoulakis.advent2021.day4

import de.choustoulakis.advent2021.day4.Board.{Point, Row}

case class Boards(boards: List[Board]) {
  def hit(in: String): Boards = Boards(boards.map(_.hit(in.toInt)))

  lazy val notWinners: Boards = Boards(boards.filterNot(maybeWinners.contains(_)))

  lazy val maybeWinner: Option[Board] = maybeWinners.headOption

  lazy val maybeWinners: List[Board] = boards.filter(b => b.isFinished)

  lazy val calculateRest: Int = boards.map(_.calculateRest).sum

  override def toString: String = boards.map(_.toString).reduce(_ + "\n\n" + _)
}

object Boards {
  def apply(input: Array[String]): Boards = {
    Boards(input.map { boardString =>
      val stringOfRows = boardString.split("\\n")
      Board(stringOfRows.map { rowString =>
        rowString.trim.split("\\ {1,}")
          .map(_.trim)
          .map(_.toInt)
          .map(Point(_))
          .toList.asInstanceOf[Row]
      }.toList)
    }.toList)
  }
}