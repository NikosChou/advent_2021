package de.choustoulakis.advent2021.day8

import de.choustoulakis.advent2021.SubmarineSpec

import scala.io.Source

class Day8Test extends SubmarineSpec with Day8 {
  "The day8" should {
    val input = resource
    "sum only 1 4 7 8" in {
      solve(input.toList).map(_.toString)
        .flatten
        .map(_.toString)
        .map(_.toInt)
        .filter(List(1, 4, 7, 8).contains(_))
        .length shouldBe 352
    }

    "sum all" in {
      solve(input.toList).sum shouldBe 936117
    }
  }
}
