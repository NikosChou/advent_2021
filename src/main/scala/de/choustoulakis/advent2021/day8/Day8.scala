package de.choustoulakis.advent2021.day8

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day8.Day8.*

trait Day8 extends Puzzle[List[String], List[Int]] :
  val day: Int = 8

  override def solve(in: List[String]): List[Int] =
    in.map { case s"$in | $out" => (in, out) }
      .map(t => (Lexikon(t._1), t._2.split(" ").map(_.sorted).toList))
      .map { case (lex: Lexikon, digits: List[String]) =>
        val value = digits.map(lex.decode(_))
        Screen(value(0), value(1), value(2), value(3))
      }.map(_.toInt)

object Day8:
  case class Lexikon(a: Char, b: Char, c: Char, d: Char, e: Char, f: Char, g: Char):
    private def decode(char: Char): Char = char match {
      case `a` => 'a'
      case `b` => 'b'
      case `c` => 'c'
      case `d` => 'd'
      case `e` => 'e'
      case `f` => 'f'
      case `g` => 'g'
    }

    def decode(signal: String): Digit = Digit(signal.map(decode).sorted)

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

  case class Digit(d1: Boolean, d2: Boolean, d3: Boolean, d4: Boolean, d5: Boolean, d6: Boolean, d7: Boolean, toInt: Int):
    override def toString: String =
      val as = if (d1) s" ${"a" * 4} " else " " * 6
      val bsCs = {
        val b = if (d2) "b" else "·"
        val c = if (d3) "c" else "·"
        s"$b    $c\n" * 4
      }
      val ds = if (d4) s" ${"d" * 4} " else s" ${"·" * 4} "
      val esFs = {
        val e = if (d5) "e" else "·"
        val f = if (d6) "f" else "·"
        s"$e    $f\n" * 4
      }
      val g = if (d7) s" ${"g" * 4} " else s" ${"·" * 4} "
      s"$as\n$bsCs$ds\n$esFs$g"

  object Digit:
    val (zero, one, two, three, four, five, six, seven, eight, nine) =
      ("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")

    def apply(input: String): Digit = {
      input match {
        case `zero` => Digit(true, true, true, false, true, true, true, 0)
        case `one` => Digit(false, false, true, false, false, true, false, 1)
        case `two` => Digit(true, false, true, false, true, false, true, 2)
        case `three` => Digit(true, false, true, true, false, true, true, 3)
        case `four` => Digit(false, true, true, true, false, true, false, 4)
        case `five` => Digit(true, true, false, true, false, true, true, 5)
        case `six` => Digit(true, true, false, true, true, true, true, 6)
        case `seven` => Digit(true, false, true, false, false, true, false, 7)
        case `eight` => Digit(true, true, true, true, true, true, true, 8)
        case `nine` => Digit(true, true, true, true, false, true, true, 9)
      }
    }

  case class Screen(d1: Digit, d2: Digit, d3: Digit, d4: Digit):
    val d1s = d1.toString.split("\n")
    val d2s = d2.toString.split("\n")
    val d3s = d3.toString.split("\n")
    val d4s = d4.toString.split("\n")

    val toInt = s"${d1.toInt}${d2.toInt}${d3.toInt}${d4.toInt}".toInt

    override def toString: String = (for {
      i <- 0 until 11
      res = s"${d1s(i)}    ${d2s(i)}    ${d3s(i)}   ${d4s(i)}"
    } yield res).toList.mkString("\n")

