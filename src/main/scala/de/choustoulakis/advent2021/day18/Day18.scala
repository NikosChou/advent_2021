package de.choustoulakis.advent2021.day18

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day18.Day18.*

import scala.util.Try

trait Day18 extends Puzzle[List[String], Output] :
  val day = 18

  override def solve(input: List[String]): Output =
    val nums = input.map(Tree.parseTree(_))
    val magnitude = nums.reduce((l, r) => (l + r).normalize).magnitude
    val pairs = nums.flatMap(x => nums.map(y => (x,y)))

    val max = pairs
      .map(t => (t._1 + t._2).normalize)
      .map(_.magnitude)
      .max
    Output(magnitude, max)

object Day18:
  case class Output(part1: Long, part2: Long)

