package de.choustoulakis.advent2021.day4

import de.choustoulakis.advent2021.SubmarineSpec

import scala.io.Source

class Day4Test extends SubmarineSpec with Day4 {

  "The bingo" should {
    val board: String =
      """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        |
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7""".stripMargin

    "play first win board" in {
      hitFisrt(board) shouldBe 4512
    }

    "play last win board" in {
      hitLast(board) shouldBe 1924
    }
  }

  "The bingo (Source)" should {
    val board = Source.fromResource("day4/input.txt").getLines().reduce(_ + "\n" + _)
    "play first win board" in {
      hitFisrt(board) shouldBe 23177
    }

    "play last win board" in {
      hitLast(board) shouldBe 6804
    }
  }

}
