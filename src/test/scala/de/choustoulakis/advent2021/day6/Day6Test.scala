package de.choustoulakis.advent2021.day6

import de.choustoulakis.advent2021.SubmarineSpec
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.io.Source

class Day6Test extends SubmarineSpec with Day6 {

  "The Fishes" should {
    implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
    val input = "3,4,3,1,2"
    "grow" in {
      solve(input, 80) shouldBe 5934
    }
    "grow for ever" in {
      solve(input, 256) shouldBe 26984457539L
    }
  }

    "The Fishes (resource)" should {
      lazy val input = Source.fromResource("day6/input.txt").getLines().reduce(_ + _)
      "grow" in {
        solve(input, 80) shouldBe 366057
      }
      "grow for ever" in {
        solve(input, 256) shouldBe 1653559299811L
      }
    }


}
