package de.choustoulakis.advent2021.day7

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day7.Day7.Input
import de.choustoulakis.advent2021.day7.Day7.Sum

trait Day7 extends Puzzle[Input, Int] :
  val day: Int = 7

  override def solve(in: (List[Int], Sum)): Int =
    val (input, acc) = in

    (input.min to input.max)
      .map(o => input
        .map(acc.sum(o, _))
        .sum)
      .min

object Day7:
  type Input = (List[Int], Sum)

  sealed trait Sum:
    val sum: (l: Int, r: Int) => Int

  case object SumAbs extends Sum :
    val sum = (l: Int, r: Int) => Math.abs(l - r)

  case object SumConsecutive extends Sum :
    val sum = (l: Int, r: Int) => {
      val n = Math.abs(l - r)
      (n * (n + 1)) / 2
    }

