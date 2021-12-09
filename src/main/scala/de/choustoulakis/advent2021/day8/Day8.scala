package de.choustoulakis.advent2021.day8

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day8.Day8.*

trait Day8 extends Puzzle[List[String], List[Int]] :
  val day: Int = 8

  override def solve(in: List[String]): List[Int] =
    return in.map {
      case s"$in | $out" => (in, out)
    }.map(t => (t._1, t._2.split(" ").toList))
      .map(t => (t._1, t._2.map(_.sorted)))
      .map {case (lex: String, digits: List[String]) =>
        val lexikon = Lexikon(lex)
        digits.map(s => Signal(s, lexikon).i).mkString("").toInt
      }

object Day8:
  case class Lexikon(a: Char, b: Char, c: Char, d: Char, e: Char, f: Char, g: Char):
    private def decode(char: Char): Char = char match {
      case c if c == a => 'a'
      case c if c == b => 'b'
      case c1 if c1 == c => 'c'
      case c if c == d => 'd'
      case c if c == e => 'e'
      case c if c == f => 'f'
      case c if c == g => 'g'
    }

    def decode(signal: String): Int =
      signal.map(decode).sorted match {
        case "abcefg" => 0
        case "cf" => 1
        case "acdeg" => 2
        case "acdfg" => 3
        case "bcdf" => 4
        case "abdfg" => 5
        case "abdefg" => 6
        case "acf" => 7
        case "abcdefg" => 8
        case "abcdfg" => 9
      }

  object Lexikon:
    def apply(input: String): Lexikon =
      val lexikon = input.split(" ").map(_.sorted).sortBy(_.length)
      val grouped = lexikon.flatten.groupBy(identity).map(t => (t._1, t._2.length))
      val (_1, _4, _7) = (lexikon(0), lexikon(2), lexikon(1))

      val _a = _7.filterNot(_1.contains(_)).head
      val _b = grouped.filter(_._2 == 6).head._1
      val _e = grouped.filter(_._2 == 4).head._1
      val _f = grouped.filter(_._2 == 9).head._1
      val _c = grouped.filter(_._2 == 8).filterNot(_._1 == _a).head._1
      val _d = _4.filterNot(_1.contains(_)).filterNot(_ == _b).head
      val _g = grouped.filter(_._2 == 7).filterNot(_._1 == _d).head._1
      Lexikon(_a, _b, _c, _d, _e, _f, _g)

  case class Signal(i: Int)

  object Signal:
    def apply(input: String, lexikon: Lexikon): Signal = Signal(lexikon.decode(input))
