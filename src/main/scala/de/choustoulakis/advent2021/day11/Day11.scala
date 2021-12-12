package de.choustoulakis.advent2021.day11

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day11.Day11.{Board, Output}

import scala.annotation.tailrec
import scala.util.Try

trait Day11 extends Puzzle[String, Output] :
  val day = 11

  override def solve(input: String): Output =
    val hundred = 100
    val (part1, flashedBoard) = (0 until hundred).foldLeft((0, Board(input))) { case ((flashes, board), _) =>
      val flashedBoard = board.tick
      (flashes + flashedBoard.dumbos.filter(_.value == 0).length, flashedBoard)
    }

    def flashUntilSync(b: Board, times: Int = 0): Int =
      if (b.dumbos.forall(_.value == 0)) {
        times + hundred
      } else {
        flashUntilSync(b.tick, times + 1)
      }

    Output(part1, flashUntilSync(flashedBoard))

object Day11:
  case class Board(dumbos: List[_ <: Dumbo]):
    def tick: Board = {
      val tickedBoard = Board(dumbos.map(_.tick))
      val temp = dumbos.indices.foldLeft(tickedBoard)((board, _) => board.dumbos.foldLeft(board)((b, d) => d.flash(b)))

      Board(temp.dumbos.map(d => if (d.flashed) d.copy(value = 0, flashed = false) else d))
    }

    def apply(i: Int)(j: Int): Option[Dumbo] = dumbos.find(d => d.i == i && d.j == j)

  object Board:
    def apply(input: String): Board = Board {
      input.split("\n").map(_.split(""))
        .zipWithIndex.flatMap { case (line, j) =>
        line.zipWithIndex.map { case (char, i) =>
          Dumbo(i, j, char.toInt)
        }
      }.toList
    }

  case class Dumbo(i: Int, j: Int, value: Int, flashed: Boolean = false):
    def flash(board: Board): Board =
      if (value > 9 && flashed == false) {
        val neighborsV = neighbors(board).toSet
        val rest = board.dumbos
          .filterNot(neighborsV.contains(_))
          .filterNot(t => t.i == i && t.j == j)

        Board((neighborsV.map(_.tick) ++ rest + copy(flashed = true)).toList)
      } else board


    def tick = copy(value = value + 1)

    def neighbors(board: Board): List[Dumbo] = List(
      board(i - 1)(j), board(i)(j - 1),
      board(i + 1)(j), board(i)(j + 1),
      board(i + 1)(j + 1),
      board(i + 1)(j - 1),
      board(i - 1)(j + 1),
      board(i - 1)(j - 1),
    ).flatten

  case class Output(part1: Int, part2: Int)
