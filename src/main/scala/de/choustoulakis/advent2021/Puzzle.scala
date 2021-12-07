package de.choustoulakis.advent2021

trait Puzzle[I, O]:
  val day: Int
  def solve(input: I): O
