package de.choustoulakis.advent2021.day10

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day10.Day10.*

trait Day10 extends Puzzle[List[String], Output] :
  val day = 10

  override def solve(input: List[String]): Output =
    val invalidScore = input.map(Line(_)).filter(_.isInstanceOf[InvalidLine]).map(_.score).sum
    val validScore = input.map(Line(_)).filter(_.isInstanceOf[ValidLine]).map(_.score).sorted

    Output(invalidScore, validScore(validScore.size / 2))

object Day10:
  case class Output(part1: Long, part2: Long)

  sealed trait Line:
    def mirror: Line

    def score: Long

  object Line:
    def apply(input: String): Line = {
      val normalizeValue = input.foldLeft("")((acc, c) => if (isClosingChunk(acc, c)) acc.dropRight(1) else acc + c)

      normalizeValue.exists(isClose(_)) match {
        case true => InvalidLine(normalizeValue.find(isClose(_)).get)
        case false => ValidLine(normalizeValue)
      }
    }

    val isOpen: Char => Boolean = _ match {
      case '{' => true
      case '(' => true
      case '[' => true
      case '<' => true
      case _ => false
    }

    val isClose: Char => Boolean = !isOpen(_)

    val toClose: Char => Char = _ match {
      case '{' => '}'
      case '(' => ')'
      case '[' => ']'
      case '<' => '>'
      case c => '?'
    }
    val isClosingChunk: (String, Char) => Boolean = (s, c) => s.nonEmpty && toClose(s.last) == c
    val invalidScore: Char => Int = _ match {
      case '}' => 1197
      case ')' => 3
      case ']' => 57
      case '>' => 25137
    }

    val validScore: Char => Int = _ match {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    }

  case class InvalidLine(invalidChar: Char) extends Line :
    val mirror = this
    val score = Line.invalidScore(invalidChar)

  case class ValidLine(s: String) extends Line :
    def mirror: ValidLine = ValidLine(s.map(Line.toClose(_)).reverse)

    def score = mirror.s.foldLeft(0L)((total, next) => total * 5 + Line.validScore(next))