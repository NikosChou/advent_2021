package de.choustoulakis.advent2021.day18

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day18.Day18.*

import scala.util.Try

trait Day18 extends Puzzle[String, Output] :
  val day = 18

  override def solve(input: String): Output =

    Output(0, 0)

object Day18:
  case class Output(part1: Int, part2: Long)
  
