package de.choustoulakis.advent2021.day17

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day17.Day17.*

trait Day17 extends Puzzle[String, Output] :
  val day = 17

  override def solve(input: String): Output =
    def go(p: Probe, target: Target): Option[Probe] = {
      if (target.isProbeInTarget(p)) Some(p)
      else if (target.isProbeTooFarWay(p)) None
      else go(p.step, target)
    }

    val target = input match {
      case s"target area: x=$fx..$tx, y=$ty..$fy" => Target(fx.toInt, tx.toInt, fy.toInt, ty.toInt)
    }

    val hits = for {
      i <- 1 to target.toX
      j <- target.toY to Math.abs(target.toY)
      initial = Probe(0, 0, i, j)
      maybeRes = go(initial, target)
      if (maybeRes.isDefined)
    } yield (maybeRes.get)

    val maxHit = hits.map(state => (state, state.previousStates.map(_.y).max)).maxBy(_._2)

    Output(maxHit._2, hits.size)

object Day17:
  case class Output(part1: Int, part2: Long)

  case class Probe(x: Int, y: Int, vx: Int, vy: Int, previousStates: Seq[Probe] = Nil):
    def step: Probe =
      val newX = x + vx
      val newY = y + vy
      val newVX = if (vx == 0) 0 else if (vx > 0) vx - 1 else vx + 1
      val newVY = vy - 1
      copy(newX, newY, newVX, newVY, this +: previousStates)


  case class Target(fromX: Int, toX: Int, fromY: Int, toY: Int):

    def isProbeInTarget(probe: Probe): Boolean = probe match {
      case Probe(x, y, _, _, _) if fromX <= x && x <= toX && fromY >= y && y >= toY => true
      case Probe(x, y, _, _, _) => false
    }

    def isProbeTooFarWay(probe: Probe): Boolean = probe match {
      case Probe(x, y, _, _, _) if y < toY => true
      case Probe(x, y, _, _, _) => false
    }


