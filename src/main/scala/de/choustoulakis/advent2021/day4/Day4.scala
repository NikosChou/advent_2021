package de.choustoulakis.advent2021.day4

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day4.Day4.Input

trait Day4 extends Puzzle[Input, Int]:
  val day = 4

  override def solve(in: Input): Int =
    val (input, action) = in
    action(Game(input))

object Day4:
  type Input = (String, Game => Int)
