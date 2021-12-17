package de.choustoulakis.advent2021.day14

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day14.Day14.*

import scala.util.control.Breaks

trait Day14 extends Puzzle[String, Output] {
  val day = 14

  override def solve(input: String): Output = {
    val templateInput = input.split("\n\n")(0)
    val pairs = input.split("\n\n")(1).split("\n")
      .map {
        case s"$p -> $c" => Pair(p, c.head)
      }

    def step(template: String): String = pairs.filter(p => template.contains(p.from)).foldLeft(template) { (tem, pair) =>
      def helper(s: String): String = {
        val index = s.indexOf(pair.from)
        if (index > -1) {
          val (first, last) = s.splitAt(index + 1)
          //println(s"indx: $index, pair: $pair first: $first, last: $last")
          s"$first${pair.c.toLower}${helper(last)}"
        } else {
          s
        }
      }

      helper(tem)
    }.toUpperCase()

    val steps = (1 to 10).foldLeft(templateInput)((tem, _) => {
      step(tem)
    })


    var map = collection.mutable.Map(templateInput.zip(templateInput.tail).map { case (l, r) => (s"$l$r" -> 1L) }
      .foldLeft(List[(String, Long)]())((acc, ne) => {
        if(acc.contains(ne)) {
          (ne._1, (ne)._2 +1) :: acc.filterNot(_ == ne)
        }else {
          ne :: acc
        }
      })
      .toMap.toSeq: _*).withDefaultValue(0L)
    println(map)

    (0 until 40).foreach { _ =>
      val newMap = collection.mutable.Map.from(map).withDefaultValue(0L)
        pairs.foreach { p =>
          if (map.contains(p.from)) {
            //println(s"$p $newMap  $map")
            val v = map(p.from)
            newMap(p.from) -= v
            newMap(p.from.head + p.c.toString) += v
            newMap(p.c.toString + p.from.last) += v
          }
        }
        map = newMap
    }
    var count = collection.mutable.Map[Char, Long]().withDefaultValue(0L)
    pairs.foreach(p => {
      count(p.from.head) += map(p.from)
      count(p.from.last) += map(p.from)
    })

    count(templateInput.head) += 1
    count(templateInput.last) += 1
    count = count.map(t => (t._1, t._2/2))
    println(count.toSeq.sortBy(_._2))

    Output(steps.groupBy(identity).maxBy(_._2.length)._2.length - steps.groupBy(identity).minBy(_._2.length)._2.length,
      (count.maxBy(_._2)._2) - (count.minBy(_._2)._2))
  }

}

object Day14:
  case class Output(part1: Int, part2: Long)

  case class Pair(from: String, c: Char)
