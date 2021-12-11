package de.choustoulakis.advent2021.day10

import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.io.Source

class Day10Test extends SubmarineSpec with Day10 {
  "The Day10" should {
    val input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]".split("\n").toList
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 26397
      }

    "solve part 2" in {
      result.part2 shouldBe 288957
    }
  }

  "The Day10 Resource" should {
    val input = resource
    val result = solve(input)
    "solve part 1" in {
      result.part1 shouldBe 469755
      }

    "solve part 2" in {
      result.part2 shouldBe 2762335572L
    }
  }

}
