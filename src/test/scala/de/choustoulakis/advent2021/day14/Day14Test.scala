package de.choustoulakis.advent2021.day14

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day14Test extends SubmarineSpec with Day14 {
  "The Day14" should {
    val input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 1588
    }

    "solve part 2" in {
      result.part2 shouldBe 2188189693529L
    }
  }

  "The Day14 Resource" should {
    val input = resource.mkString("\n")
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 3095
    }

    "solve part 2" in {
      //                    2489051461163
      result.part2 shouldBe 3152788426516L
    }
  }

}
