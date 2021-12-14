package de.choustoulakis.advent2021.day12

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day12.Day12.*

trait Day12 extends Puzzle[String, Output] :
  val day = 12

  override def solve(input: String): Output =
    val caves = input.split("\n").map { case s"$l-$r" => (Cave(l), Cave(r)) }
    val nextCaves = next(_, caves)

    def findPath(cave: Cave, path: Path, part2Filter: Part2Filter): Seq[Path] =
      def doNext(c: Cave, p: Path): Seq[Path] = nextCaves(c).flatMap(ca => findPath(ca, p :+ c, part2Filter))

      cave match {
        // @formatter:off
        case End => List(path :+ End)
        case cave if !path.contains(cave) =>          doNext(cave, path)
        case cave: SmallCave if part2Filter(path) =>  doNext(cave, path)
        case cave: BigCave =>                         doNext(cave, path)
        case _ => Nil
        // @formatter:on
      }

    Output(findPath(Start, Nil, ignorePart2Filter).length, findPath(Start, Nil, part2Filter).length)

object Day12:
  type Path = Seq[Cave]
  type Part2Filter = Path => Boolean

  val next: (Cave, Array[(Cave, Cave)]) => Path = (c, caves) => (caves.filter(t => t._1 == c).map(_._2) ++: caves.filter(t => t._2 == c).map(_._1))
  val part2Filter: Part2Filter = path => !path.filter(_.isInstanceOf[SmallCave]).groupBy(identity).exists(_._2.length == 2)
  val ignorePart2Filter: Part2Filter = _ => false

  case class Output(part1: Int, part2: Int)

  sealed trait Cave

  object Cave:
    def apply(in: String): Cave =
      if (in == "start") Start
      else if (in == "end") End
      else if (in.head.isLower) SmallCave(in)
      else BigCave(in)

  case object Start extends Cave :
    override def toString: String = "start"

  case object End extends Cave :
    override def toString: String = "end"

  case class BigCave(name: String) extends Cave :
    override def toString: String = name

  case class SmallCave(name: String) extends Cave :
    override def toString: String = name
