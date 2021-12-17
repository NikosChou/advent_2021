package de.choustoulakis.advent2021.day15

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day15.Day15.*
import scala.collection.mutable
import scala.util.Try

trait Day15 extends Puzzle[List[String], Output]{
  override val day: Int = 15

  override def solve(in: List[String]): Output = {
    //TODO restore array from input_
    val input = in.toArray.map(_.split("").map(_.toInt))
    val points = for {
      i <- input.indices
      j <- input(i).indices
    } yield Point(i, j, input(i)(j))

    val graph: Graph[Point] = p => Set(
      (p.i - 1, p.j), (p.i, p.j - 1),
      (p.i + 1, p.j), (p.i, p.j + 1)
    ).collect { case (i, j) => Try((Point(i, j, input(i)(j)))).toOption }
      .flatten

    val first = points.head
    val goal = points.last

    Output(Day15.shortestPathCost(graph)(first, goal), 0)
  }
}

object Day15:
  case class Output(part1: Int, part2: Int)
  case class Point(i: Int, j: Int, w: Int):
    def h(goal: Point): Int = {
      val i1 = Math.abs(i - goal.i)
      val i2 = Math.abs(j - goal.j)
      i1 + i2
    }

  type Graph[N] = N => Set[N]


  def shortestPathCost(g: Graph[Point])(source: Point, target: Point): Int = {
    def helper(tuple: (Int, Point), active: mutable.PriorityQueue[(Int, Point)], acc: Set[Point]): Int = {
      val (cost, point) = tuple
      if (point == target) cost
      else if (acc.contains(point)) helper(active.dequeue(), active, acc)
      else {
        g(point).map(p => (cost + p.w, p)).foreach(active.enqueue(_))
        helper(active.dequeue(), active, acc + point)
      }
    }

    val active = mutable.PriorityQueue[(Int, Point)]()(Ordering.by(-_._1))
    helper((0, source), active, Set())
  }
