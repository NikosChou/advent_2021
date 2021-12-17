package de.choustoulakis.advent2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source
import scala.util.{Failure, Success, Try}

class SubmarineSpec extends AnyWordSpec with Matchers {
  self: Puzzle[_, _] =>
  lazy val resource = {
    Try(Source.fromResource(s"day${self.day}/input.txt")).map(_.getLines().toList) match {
      case Success(r) => r
      case Failure(_) => ???
//        import scala.language.postfixOps
//        import sys.process._
//        import java.net.URL
//        import java.io.File
//        new URL(s"https://adventofcode.com/2021/day/${self.day}/input") #> new File(s"dinput.txt") !!;
//        Source.fromResource(s"day${self.day}/input.txt").getLines().toList
    }
  }
}