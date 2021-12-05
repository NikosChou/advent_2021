package de.choustoulakis.advent2021.day5

import scala.util.control.Breaks
import scala.util.control.Breaks.breakable

object Ground {
  sealed trait Line {
    def toPoints: List[Point]
  }

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    private val distanceFromZero = Math.sqrt((x * x) + (y * y))

    override def compare(that: Point): Int = {
      val compare = x - that.x
      if (compare == 0) {
        y - that.y
      } else compare
    }

  }

  object Point {
    def apply(input: String): Point = {
      val ints = input.split(",").map(_.toInt)
      Point(ints(0), ints(1))
    }
  }

  case class Horizontal(start: Int, end: Int, y: Int) extends Line {
    lazy val toPoints = Line.helper(Point(start, y), Point(end, y), p => Point(p.x + 1, y))
  }

  case class Vertical(start: Int, end: Int, x: Int) extends Line {
    lazy val toPoints = Line.helper(Point(x, start), Point(x, end), p => Point(x, p.y + 1))
  }

  case class Diagonal45(start: Point, end: Point) extends Line {
    lazy val toPoints = {
      val next: Point => Point = {
        if (start.x < end.x && start.y < end.y) p => Point(p.x + 1, p.y + 1)
        else p => Point(p.x + 1, p.y - 1)
      }
      Line.helper(start, end, next)
    }
  }

  case object NoLine extends Line {
    override val toPoints = Nil
  }

  object Line {
    def helper(point: Point, end: Point, next: Point => Point): List[Point] = {
      if (point == end) {
        List(end)
      } else {
        point +: helper(next(point), end, next)
      }
    }

    private def is45Degrees(start: Point, end: Point): Boolean = {
      val dx = Math.abs(start.x - end.x)
      val dy = Math.abs(start.y - end.y)
      val theta = Math.atan2(dy, dx)
      val angle = 180 / Math.PI * theta
      angle == 45
    }

    def apply(input: String): Line = {
      val points = input.split(" -> ")
      List(Point(points(0)), Point(points(1))).sorted match {
        case List(Point(ax, ay), Point(bx, by)) if ay == by => Horizontal(ax, bx, ay)
        case List(Point(ax, ay), Point(bx, by)) if ax == bx => Vertical(ay, by, ax)
        case List(a, b) if is45Degrees(a, b) => Diagonal45(a, b)
        case _ => NoLine
      }
    }
  }
}
