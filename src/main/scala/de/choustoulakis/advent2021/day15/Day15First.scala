package de.choustoulakis.advent2021.day15

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day15.Day15First.*

import scala.annotation.tailrec
import scala.util.Try

trait Day15First extends Puzzle[String, Output] :
  override val day: Int = 15

  override def solve(input: String): Output =
    val board = Board(input)
    val head = board.points.flatten.head
    val last = board.points.flatten.last
    val sDistances = board.points.flatten.map(p => (p, Int.MaxValue)).toMap + (head -> 0)

    @tailrec
    def shortestPath(step: ShortStep, last: Point): ShortStep = {
      if (step.unProcessed == Set()) step
      else {
        if (step.unProcessed.size % 1000 == 0) println(step.unProcessed.size)
        val (n, newDists, newParents) = step.extractMin().map { case (n, currentD) =>
          val newDists = n.neighbors.collect {
            case point if step.dists.get(point).exists(_ > currentD + point.weight) => point -> (currentD + point.weight)
          }
          val newParents = newDists.map { case (point, weight) => point -> n }
          (n, newDists, newParents)
        }.getOrElse(head, Nil, Nil)

        val unPr = step.unProcessed - n
        shortestPath(ShortStep(step.parents ++ newParents, unPr, step.dists ++ newDists), last)
      }
    }

    def extractPaths(point: Point, parents: Map[Point, Point]): List[Point] = {
      parents.get(point).map(p => point +: extractPaths(p, parents)).getOrElse(List(point))
    }

    val foo = shortestPath(ShortStep(Map(), sDistances.keys.toSet, sDistances), last)
    val extracted = extractPaths(last, foo.parents)

    val list = input.split("\n").toList
    val value = (1 until 5).foldLeft(List(list)) { case (acc, _) => acc :+ acc.last
      .foldLeft(List[String]()) { case (_, line) => (acc.last.map { l =>
        l.map { c =>
          val i = c.toString.toInt
          if (i == 9) 1 else i + 1
        }.mkString("")
      }).toList
      }
    }
    val input2 = value.tail.foldLeft(value.head) { (acc, lines) =>
      acc.zip(lines).map(_ + _)
    }

    val inputPart2 = (1 until 5).foldLeft(List(input2)) { case (acc, _) => {
      acc :+ acc.last
        .foldLeft(List[String]()) { case (_, line) => (acc.last.map { l =>
          l.map { c =>
            val i = c.toString.toInt
            if (i == 9) 1 else i + 1
          }.mkString("")
        }).toList
        }
    }
    }.map(_.mkString("\n")).mkString("\n")


    val board2 = Board(inputPart2)
    val head2 = board2.points.flatten.head
    val last2 = board2.points.flatten.last
    val sDistances2 = board2.points.flatten.map(p => (p, Int.MaxValue)).toMap + (head2 -> 0)


    val foo2 = shortestPath(ShortStep(Map(), sDistances2.keys.toSet, sDistances2), last2)
    val extracted2 = extractPaths(last2, foo2.parents)

    Output(extracted.map(_.weight).sum - head.weight,
      extracted2.map(_.weight).sum - head.weight
    )

object Day15First:
  case class Output(part1: Long, part2: Long)

  case class ShortStep(parents: Map[Point, Point], unProcessed: Set[Point], dists: Map[Point, Int]) {
    def extractMin(): Option[(Point, Int)] = {
      Try(unProcessed.minBy(n => dists(n))).toOption
        .map(n => (n, dists(n)))
    }
  }

  case class Board(private val input: String):
    val points: Array[Array[Point]] = input.split("\n").map(_.split(""))
      .zipWithIndex.map { case (line, i) =>
      line.zipWithIndex.map { case (char, j) =>
        Point(i, j, char.toString.toInt, this)
      }
    }

    def apply(i: Int)(j: Int): Option[Point] = Try(points(i)(j)).toOption

  case class Node(point: Point, w: Int = Int.MaxValue)

  case class Point(i: Int, j: Int, weight: Int, board: Board) extends Ordered[Point] :
    val neighbors: List[Point] = List(
      board(i)(j + 1), board(i + 1)(j),
      board(i)(j - 1), board(i - 1)(j)
    ).flatten

    override def compare(that: Point): Int = weight.compare(that.weight)

    override def toString: String = (i, j, weight).toString()

