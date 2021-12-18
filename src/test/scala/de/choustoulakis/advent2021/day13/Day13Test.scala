package de.choustoulakis.advent2021.day13

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day13Test extends SubmarineSpec with Day13 {
  "The Day13" should {
    val input = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 17
    }

    "solve part 2" in {
      result.part2 shouldBe """#####
                              |#...#
                              |#...#
                              |#...#
                              |#####
                              |""".stripMargin
    }
  }

  "The Day13 Resource" should {
    val input = resource.mkString("\n")
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 743
    }

    "solve part 2" in {
      result.part2 shouldBe """###...##..###..#.....##..#..#.#..#.#...
                              |#..#.#..#.#..#.#....#..#.#.#..#..#.#...
                              |#..#.#....#..#.#....#..#.##...####.#...
                              |###..#....###..#....####.#.#..#..#.#...
                              |#.#..#..#.#....#....#..#.#.#..#..#.#...
                              |#..#..##..#....####.#..#.#..#.#..#.####
                              |""".stripMargin
    }
  }

}
