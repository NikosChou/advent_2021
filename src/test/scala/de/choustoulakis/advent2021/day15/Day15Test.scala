package de.choustoulakis.advent2021.day15

import de.choustoulakis.advent2021.day12.Day12
import de.choustoulakis.advent2021.day15.Day15Test.Point
import de.choustoulakis.advent2021.day15.Dijkstra.Graph
import de.choustoulakis.advent2021.{Puzzle, SubmarineSpec}

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day15Test:
  case class Point(i: Int, j: Int, w: Int):
    def h(goal: Point): Int = {
      val i1 = Math.abs(i - goal.i)
      val i2 = Math.abs(j - goal.j)
      i1 + i2
    }

object Dijkstra {
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

}

class Day15Test extends SubmarineSpec with Puzzle[List[String], Int] {

  override val day: Int = 15

  override def solve(in: List[String]): Int = {
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
    Dijkstra.shortestPathCost(graph)(first, goal)
  }

  "The Day15" should {
    "solve part 1" in {
      val input = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581".split("\n").toList
      solve(input) shouldBe 40
    }

    "solve part 2" in {
      solve(resource) shouldBe 2849
    }
  }

}
