package de.choustoulakis.advent2021.day15

import de.choustoulakis.advent2021.day15.*
import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.collection.mutable
import scala.io.Source
import scala.util.Try


class Day15Test extends SubmarineSpec with Day15 {
  "The Day15" should {
    val input = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581".split("\n").toList
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 40
    }
    "solve part 2" in {
      result.part2 shouldBe 40
    }
  }

  "The Day15 Resource" should {
    val result = solve(resource)
    "solve part 1" in {
      result.part1 shouldBe 2849
    }

    "solve part 2" in {
      result.part2 shouldBe 2849
    }
  }

}
