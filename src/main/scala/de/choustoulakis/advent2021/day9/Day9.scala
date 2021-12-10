package de.choustoulakis.advent2021.day9

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day9.Day9.{Board, Output, Point}

import scala.util.Try

trait Day9 extends Puzzle[String, Output] :
  val day = 9

  override def solve(input: String): Output =
    val board = Board(input)
    val minPoints = board.points.flatten.filter(p => p.neighbors.min > p)
    val part1 = minPoints.map(_.value + 1).sum
    val part2 = minPoints.map(_.basin.size).sorted.takeRight(3).product
    Output(part1, part2)

object Day9:
  case class Board(private val input: String):
    val points: Array[Array[Point]] = input.split("\n").map(_.split(""))
      .zipWithIndex.map { case (line, i) =>
      line.zipWithIndex.map { case (char, j) =>
        Point(i, j, char.toInt)(this)
      }
    }

    def apply(i: Int)(j: Int): Option[Point] = Try(points(i)(j)).toOption

  case class Point(i: Int, j: Int, value: Int)(board: Board) extends Ordered[Point] :

    def neighbors: List[Point] = List(
      board(i - 1)(j), board(i)(j - 1),
      board(i + 1)(j), board(i)(j + 1)
    ).flatten

    def basin: Set[Point] = {
      def helper(p: Point, acc: Set[Point] = Set()): Set[Point] = p.neighbors.toSet
        .filter(_.value < 9)
        .filterNot(acc.contains(_))
        .foldLeft(acc + p)((a, c) => helper(c, a + c))

      helper(this)
    }

    override def compare(that: Point): Int = value.compare(that.value)

  case class Output(part1: Int, part2: Int)
