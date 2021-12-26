package de.choustoulakis.advent2021.day20

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day20.Day20.*

trait Day20 extends Puzzle[Seq[String], Output] {
  override val day: Int = 20

  override def solve(input: Seq[String]): Output = {
    val enhance: Int => Char = i => input(0)(i)
    val imageInput = input.drop(2).map(_.toArray).toArray

    def helper(image: Image, times: Int): Image = {
      if (times == 0) image
      else {
        def getPixelFor(i: Int, j: Int) = if (times % 2 == 1) getPixelvalue(enhance(0))(image)(i, j) else getPixelvalue('.')(image)(i, j)

        val enhanced = for {
          j <- -1 to image.size
          i <- -1 to image.size
          binary = (for (jj <- -1 to 1; ii <- -1 to 1) yield getPixelFor(j + jj, i + ii))
          index = Integer.parseInt(binary.mkString(""), 2)
        } yield enhance(index)

        val imageArray = enhanced.grouped(image.size + 2).map(_.toArray).toArray
        helper(imageArray, times - 1)
      }
    }

    val image = helper(imageInput, 2)
    val imagePart2 = helper(imageInput, 50)
    Output(countLit(image), countLit(imagePart2))
  }
}

object Day20 {
  case class Output(part1: Int, part2: Int)

  type Image = Array[Array[Char]]

  def getPixelvalue(defChar: Char)(img: Image): (Int, Int) => Char = (i, j) => img.lift(i).flatMap(_.lift(j)).getOrElse(defChar) match {
    case '#' => '1'
    case _ => '0'
  }

  val countLit: Image => Int = image => image.flatten.count(_ == '#')
}
