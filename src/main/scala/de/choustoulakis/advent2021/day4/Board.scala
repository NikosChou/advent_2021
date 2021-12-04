package de.choustoulakis.advent2021.day4

import de.choustoulakis.advent2021.day4.Board._

case class Board(rows: List[Row], lastHittedPoint: Int = -1) {
  def hit(in: Int): Board = Board(rows.map(_.map(p => p.hit(in))), in)

  def isFinished: Boolean = {
    def lineCompleted(line: List[List[Point]]): Boolean = line.exists(l => l.forall(_.hitted))

    val columns: List[Column] = (0 until (rows.head.length))
      .map(i => rows.map(_ (i)))
      .toList

    lineCompleted(rows) || lineCompleted(columns)
  }

  val calculateRest: Int = {
    val sumOfNotHittedPoints = rows.flatten.filterNot(_.hitted).map(_.n).sum
    sumOfNotHittedPoints * lastHittedPoint
  }

  override def toString: String = {
    (for {
      row <- rows
      toPrint = for {
        point <- row
        value = point.toString
      } yield value
    } yield toPrint.reduce(_ + " " + _)).reduce(_ + " \n" + _)
  }
}

object Board {
  case class Point(n: Int, hitted: Boolean = false) {
    def hit(in: Int): Point = if (in == n) Point(n, true) else this

    override def toString: String = if (hitted) s"|$n|" else n.toString
  }

  type Input = List[String]
  type Row = List[Point]
  type Column = List[Point]
}
