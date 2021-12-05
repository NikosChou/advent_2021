package de.choustoulakis.advent2021.day5

import de.choustoulakis.advent2021.SubmarineSpec
import de.choustoulakis.advent2021.day5.Ground.{Line, Point}

import scala.io.Source

class Day5Test extends SubmarineSpec with Day5 {
  "The Hydrothermal Venture" should {
    val input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
      .split("\\n")
      .map(Line(_))
      .toList

    "points where at least two lines overlap Vertical & Horizontal" in {
      solve(input) shouldBe 5
    }

    "points where at least two lines overlap Vertical & Horizontal & Diahonal" in {
      solve(input, l => l.isHorizontal || l.isVertical || l.isDiagonal) shouldBe 12
    }

    "The Hydrothermal Venture Resource" should {
      val input = Source.fromResource("day5/input.txt").getLines()
        .map(Line(_))
        .toList
      "points where at least two lines overlap" in {
        solve(input) shouldBe 7468
      }

      "points where at least two lines overlap Vertical & Horizontal & Diahonal" in {
        solve(input, l => l.isHorizontal || l.isVertical || l.isDiagonal) shouldBe 22364
      }
    }
  }
}