package de.choustoulakis.advent2021.day9

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day9.Day9.{Output, Point}

trait Day9 extends Puzzle[String, Output] :
  val day = 9

  override def solve(input: String): Output =
    val strings = input.split("\n")

    def get(i: Int, j: Int): Point = {
      val value = strings(i)(j)
      Point(i, j, value.toString.toInt)
    }

    def getNeighbors(p: Point): List[Point] =
      import scala.util.Try
      List(
        Try(get(p.i - 1, p.j)),
        Try(get(p.i + 1, p.j)),
        Try(get(p.i, p.j - 1)),
        Try(get(p.i, p.j + 1))
      ).filter(_.isSuccess).map(_.get)

    def allNeighborsRecurs(p: Point, acc: Set[Point] = Set()): Set[Point] = {
      getNeighbors(p).toSet
        .filter(_.value < 9)
        .filterNot(acc.contains(_))
        .foldLeft(acc + p)((a, c) => allNeighborsRecurs(c, a + c))
    }

    val part1 = for {
      i <- strings.indices
      j <- strings(0).indices
      point = get(i, j)
      neighbors = getNeighbors(point)
      if neighbors.map(_.value).min < 9
      if point.value < neighbors.minBy(_.value).value
    } yield point

    val part2 = (for {
      sm <- part1
    } yield allNeighborsRecurs(sm))
      .map(_.size)
      .sorted
      .reverse
      .take(3)
      .product

    Output(part1.map(_.value + 1).sum, part2)

object Day9:
  case class Point(i: Int, j: Int, value: Int)

  case class Output(part1: Int, part2: Int)
