package de.choustoulakis.advent2021.day5

import scala.util.control.Breaks
import scala.util.control.Breaks.breakable

object Ground {
  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {
    val isVertical: Boolean = start.y == end.y
    val isHorizontal: Boolean = start.x == end.x
    lazy val isDiagonal: Boolean = {
      val dx = maxX - minX
      val dy = maxY - minY
      val theta = Math.atan2(dy, dx)
      val angle = 180 / Math.PI * theta
      angle % 45 == 0
    }
    val maxX = if (start.x > end.x) start.x else end.x
    val maxY = if (start.y > end.y) start.y else end.y
    val minX = if (start.x < end.x) start.x else end.x
    val minY = if (start.y < end.y) start.y else end.y

    val toPoints: List[Point] = {
      if (isHorizontal) {
        for {
          y <- minY to maxY
        } yield Point(start.x, y)
      } else if (isVertical) {
        for {
          x <- minX to maxX
        } yield Point(x, start.y)
      } else if (isDiagonal) {
        def helper(startP: Point, endP: Point, next: Point => Point): List[Point] = {
          if (startP == endP) {
            endP :: Nil
          } else {
            startP +: helper(next(startP), endP, next)
          }
        }
        val next: Point => Point =
          if(start.x < end.x && start.y < end.y) { p => Point(p.x + 1, p.y + 1) }
          else if(start.x > end.x && start.y < end.y) { p => Point(p.x - 1, p.y + 1) }
          else if(start.x < end.x && start.y > end.y) { p => Point(p.x + 1, p.y - 1) }
          else { p => Point(p.x - 1, p.y - 1) }
        helper(start, end, next)
      } else {
        Nil
      }
    }.toList
  }


  object Point {
    def apply(input: String): Point = {
      val ints = input.split(",").map(_.toInt)
      Point(ints(0), ints(1))
    }
  }

  object Line {
    def apply(input: String): Line = {
      val points = input.split(" -> ")
      Line(Point(points(0)), Point(points(1)))
    }
  }
}
