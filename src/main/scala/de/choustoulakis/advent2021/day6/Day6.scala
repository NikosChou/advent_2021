package de.choustoulakis.advent2021.day6

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day6.Day6.{Input, Population}

trait Day6 extends Puzzle[Input, Long] :
  val day: Int = 6

  override def solve(in: Input): Long =
    val (input, days) = in
    val fishes = input.split(",").map(_.toInt)
    val population = Population(fishes.groupBy(identity).map {
      case (typ, fs) => (typ -> fs.length.toLong)
    })

    def helper(p: Population, daysLeft: Int): Population =
      if (daysLeft == 0) p else helper(p.tick(), daysLeft - 1)

    helper(population, days).sumPopulation

object Day6:
  type Input = (String, Int)

  case class Population(private var fishes: Map[Int, Long]):
    fishes = fishes.withDefaultValue(0L)

    def tick() = Population {
      fishes.map {
        case (0, i) => (8, i)
        case (f, i) => (f - 1, i)
      }.+(6 -> (fishes(7) + fishes(0)))
    }

    lazy val sumPopulation = fishes.foldLeft(0L)(_ + _._2)
