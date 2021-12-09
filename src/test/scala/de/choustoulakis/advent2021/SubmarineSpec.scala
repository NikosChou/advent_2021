package de.choustoulakis.advent2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class SubmarineSpec extends AnyWordSpec with Matchers { self: Puzzle[_, _] =>
  lazy val resource = Source.fromResource(s"day${self.day}/input.txt").getLines().toList
}