package de.choustoulakis.advent2021.day13

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day13.Day13.*

trait Day13 extends Puzzle[String, Output] {
  val day = 13


  def printPoints(points: Array[Point]): String = {
    var part2: String = ""
    for {
      j <- 0 to points.map(_.j).max
      i <- 0 to points.map(_.i).max
      str = if (points.find(p => p.i == i && p.j == j).get.contains) '#' else '.'
      _ = part2 += str.toString
      if (i == points.map(_.i).max)
        _ = part2 += "\n"
    } yield ()
    part2
  }

  override def solve(input: String): Output = {
    val paper = input.split("\n\n")(0)
    val instructionsIn = input.split("\n\n")(1)

    val points = paper.split("\n").map { case s"$i,$j" => Point(i.toInt, j.toInt, true) }
    def enhancedPoints(points: Seq[Point]) = (for {
      i <- 0 to points.map(_.i).max
      j <- 0 to points.map(_.j).max
      point = points.find(p => p.i == i && p.j == j).orElse(Some(Point(i, j, false)))
    } yield point).flatten.toArray

    val instructions = instructionsIn.split("\n").map {
      case s"fold along y=$y" => Horizontal(y.toInt)
      case s"fold along x=$x" => Vertical(x.toInt)
    }
    
    def fold(instr: Seq[Instructions], points: Seq[Point]) = instr.foldLeft(points) {
      case (pointss, Horizontal(y)) =>
        val res = pointss.filter(_.contains).foldLeft(Seq[Point]())((acc, p) => {
          if (p.j == y) acc
          else if (p.j < y) acc.:+(p)
          else {
            val j = y - (p.j - y)
            val ppp = pointss.find(pp => pp.i == p.i && pp.j == j).get
            val point = p.copy(j = j, contains = ppp.contains || p.contains)
            acc.filterNot(pp => pp.i == point.i && pp.j == point.j).:+(point)
          }
        })
        enhancedPoints(res)

      case (pointss, Vertical(x)) =>
        val res = pointss.filter(_.contains).foldLeft(Seq[Point]())((acc, p) => {
          if (p.i == x) acc
          else if (p.i < x) acc.:+(p)
          else {
            val i = x - (p.i - x)
            val ppp = pointss.find(pp => pp.i == i && pp.j == p.j).get
            val point = p.copy(i = i, contains = ppp.contains || p.contains)
            acc.filterNot(pp => pp.i == point.i && pp.j == point.j).:+(point)
          }
        })
        enhancedPoints(res)
    }

    val part1 = fold(instructions.head :: Nil, enhancedPoints(points))
    val part2 = fold(instructions, enhancedPoints(points))
    val part2Str = printPoints(part2.toArray)
    

    Output(part1.filter(_.contains).size, part2Str)
  }
}

object Day13:
  case class Output(part1: Int, part2: String)

  case class Point(i: Int, j: Int, contains: Boolean = false)

  sealed trait Instructions

  case class Horizontal(y: Int) extends Instructions

  case class Vertical(y: Int) extends Instructions