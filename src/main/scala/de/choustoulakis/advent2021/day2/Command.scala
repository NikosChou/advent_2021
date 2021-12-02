package de.choustoulakis.advent2021.day2

sealed trait Command

case class Forward(value: Int) extends Command

case class Up(value: Int) extends Command

case class Down(value: Int) extends Command

object Command {
  def apply(input: String): Command = input.toLowerCase.split(" ").toList match {
    case "forward" :: value :: Nil => Forward(value.toInt)
    case "up" :: value :: Nil => Up(value.toInt)
    case "down" :: value :: Nil => Down(value.toInt)
    case _ => ???
  }
}
