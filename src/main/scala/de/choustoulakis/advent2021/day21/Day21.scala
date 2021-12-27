package de.choustoulakis.advent2021.day21

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day21.Day21.*

trait Day21 extends Puzzle[String, Output] {
  override val day: Int = 21

  override def solve(input: String): Output = {
    val game = Game(input)

    def loop1(game: Game): Game = {
      if (game.isFinishedPart1.isDefined) game
      else {
        loop1(game.play)
      }
    }

    val cache: collection.mutable.Map[Game, (ID, Long)] = collection.mutable.Map()

    def loop2(game: Game): collection.mutable.Map[ID, Long] = {
      if (game.player1.score > 20) {
        collection.mutable.Map(game.player1.id -> 1L)
      } else if (game.player2.score > 20) {
        collection.mutable.Map(game.player2.id -> 1L)
      } else if (cache.contains(game)) {
        collection.mutable.Map(cache(game))
      } else {
        game.quantumPlay.foldLeft(collection.mutable.Map[ID, Long]().withDefaultValue(0L)) { case (ac, game) =>
          ac ++= loop2(game) map {
            case (id, v) =>
              cache += (game -> (id, v))
              (id -> (ac(id) + v))
          }
        }
      }
    }

    val game2 = game.copy(dice = QuantumDice(), gameEnds = 21)

    Output(loop1(game).isFinishedPart1.get, loop2(game2).map(_._2).max)
  }
}

object Day21 {

  case class Output(part1: Int, part2: Long)

  sealed trait Dice {
    def value: Int

    def thrw: Dice

    def timesThrowed: Int
  }

  case class DeterministicDice(value: Int = 0, timesThrowed: Int = 0) extends Dice {
    override def thrw: Dice = {
      DeterministicDice(value + 1, timesThrowed + 1)
    }
  }

  case class QuantumDice(value: Int = 0) extends Dice {
    override def timesThrowed: Int = -1

    override def thrw: Dice = {
      QuantumDice(if (value == 3) 1 else value + 1)
    }
  }

  enum ID {
    case P1, P2
  }

  case class Player(id: ID, pos: Int, score: Int) {
    def nextPos(moves: Int): Player = {

      def loop(i: Int): Int = {
        if (i > 10) loop(i - 10)
        else i
      }

      val newPos = loop(pos + moves)
      Player(id, newPos, score + newPos)
    }
  }

  case class Game(player1: Player, player2: Player, dice: Dice, gameEnds: Int = 1000) {
    def play: Game = {
      val (newDice, moveForward) = (1 to 3).foldLeft((dice, 0)) { case ((d, s), _) =>
        val dd = d.thrw
        (dd, s + dd.value)
      }
      val newPlayer1 = player1.nextPos(moveForward)
      Game(player2, newPlayer1, newDice, gameEnds)
    }

    def quantumPlay: Seq[Game] = {
      if (isFinishedPart2.isDefined) Seq()
      else {
        val times = for {
          i <- 1 to 3
          j <- 1 to 3
          z <- 1 to 3
        } yield (i + j + z)
        times.map { move =>
          val newPlayer1 = player1.nextPos(move)
          Game(player2, newPlayer1, dice, gameEnds)
        }
      }
    }

    val getPlayer1: Player = (player1, player2) match {
      case (p@Player(ID.P1, _, _), _) => p
      case (_, p@Player(ID.P1, _, _)) => p
    }
    val getPlayer2: Player = (player1, player2) match {
      case (p@Player(ID.P2, _, _), _) => p
      case (_, p@Player(ID.P2, _, _)) => p
    }

    val isFinishedPart2: Option[ID] = if (player1.score < 21 && player2.score < 21) None else {
      (getPlayer1, getPlayer2) match {
        case (Player(_, _, score1), Player(_, _, score2)) if score1 > 20 => Some(ID.P1)
        case (Player(_, _, score1), Player(_, _, score2)) if score2 > 20 => Some(ID.P2)
        case _ => None
      }
    }

    val isFinishedPart1: Option[Int] = (player1, player2) match {
      case (Player(_, _, score1), Player(_, _, score2)) if score1 > 999 => Some(score2 * dice.timesThrowed)
      case (Player(_, _, score1), Player(_, _, score2)) if score2 > 999 => Some(score1 * dice.timesThrowed)
      case _ => None
    }
  }

  object Game {
    def apply(input: String): Game = {
      val players = input.split("\n").map {
        case s"Player 1 starting position: $pos" => Player(ID.P1, pos.toInt, 0)
        case s"Player 2 starting position: $pos" => Player(ID.P2, pos.toInt, 0)
      }
      Game(players(0), players(1), DeterministicDice())
    }
  }
}
