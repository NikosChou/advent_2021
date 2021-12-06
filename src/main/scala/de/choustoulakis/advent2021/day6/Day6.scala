package de.choustoulakis.advent2021.day6

import de.choustoulakis.advent2021.day6.Day6.Population

trait Day6:
  def solve(input: String, days: Int): Long =
    val fishes = input.split(",").map(_.toInt)
    val population = Population(fishes.groupBy(f => f).map(t => (t._1 -> t._2.length.toLong)))

    def helper(p: Population, daysLeft: Int): Population =
      if (daysLeft == 0) p else helper(p.passDay(), daysLeft - 1)

    helper(population, days).sumPopulation

object Day6:
  case class Population(fishes: Map[Int, Long]):
    def passDay() =
      val readyToBorn = fishes.getOrElse(0, 0L)
      val newPop = fishes.map {
        case (0, i) => (8, i)
        case (f, i) => (f - 1, i)
      }.+(6 -> (fishes.getOrElse(7, 0L) + readyToBorn))
      Population(newPop)

    lazy val sumPopulation = fishes.foldLeft(0L)(_ + _._2)
