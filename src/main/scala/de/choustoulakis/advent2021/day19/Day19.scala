package de.choustoulakis.advent2021.day19

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day19.Day19.*

trait Day19 extends Puzzle[List[String], Output] {
  override val day: Int = 19

  override def solve(input: List[String]): Output = {
    val scannerInputs = input.mkString("\n").split("\n\n")
    val scanners = scannerInputs.map(Scanner.apply).toList

    val pairOfScanners = for {
      (s1, idx1) <- scanners.zipWithIndex
      (s2, idx2) <- scanners.zipWithIndex
      if idx1 < idx2
    } yield (s1, s2)


    def loop(scannerAcc: Scanner, nextScanners: List[Scanner]): Scanner = {
      if (nextScanners.isEmpty) scannerAcc
      else {
        val scannerHead = nextScanners.head
        println(scannerHead.name)
        val maybe = (for {
          (scanner1, scanner2) <- List((scannerAcc, scannerHead))
          c1 <- scanner1.distancesPerBeacon.values
          distances1 = c1.map(_._2).sorted
          c2 <- scanner2.beacons.map(_.permutations).transpose.map(bs => scanner2.copy(beacons = bs)).map(_.distancesPerBeacon).flatMap(_.values)
          distances2 = c2.map(_._2).sorted
          if distances1.intersect(distances2).size > 11
          c1Head = c1.head._1
          c2Head <- c2.head._1.permutations
          candidate = c1Head - c2Head
          if(c1.map(_._1).intersect(c2.map(_._1).map(_ + candidate)).size > 11)
        } yield (c2.map(_._1).map(_ + candidate))).headOption

        if (maybe.isDefined) {
          val common1 = maybe.get


          val foo = scannerAcc.copy(beacons = (scannerAcc.beacons ++ common1).distinct)
          loop(foo, nextScanners.tail)
        } else {
          loop(scannerAcc, nextScanners.tail :+ scannerHead)
        }
      }
    }

    val scannerZero = scanners.head.copy(coordinate = Some(Coordinate3D(0, 0, 0)))
    val scanner = loop(scannerZero, scanners.tail)

    Output(scanner.beacons.size, 0)
  }
}

object Day19 {
  case class Output(part1: Int, part2: Int)

  case class Coordinate3D(x: Int, y: Int, z: Int) extends Ordered[Coordinate3D] {
    def permutations: Seq[Coordinate3D] = Seq(
      Coordinate3D(x, y, z), Coordinate3D(y, z, x), Coordinate3D(z, x, y), Coordinate3D(-x, z, y),
      Coordinate3D(z, y, -x), Coordinate3D(y, -x, z), Coordinate3D(x, z, -y), Coordinate3D(z, -y, x),
      Coordinate3D(-y, x, z), Coordinate3D(x, -z, y), Coordinate3D(-z, y, x), Coordinate3D(y, x, -z),
      Coordinate3D(-x, -y, z), Coordinate3D(-y, z, -x), Coordinate3D(z, -x, -y), Coordinate3D(-x, y, -z),
      Coordinate3D(y, -z, -x), Coordinate3D(-z, -x, y), Coordinate3D(x, -y, -z), Coordinate3D(-y, -z, x),
      Coordinate3D(-z, x, -y), Coordinate3D(-x, -z, -y), Coordinate3D(-z, -y, -x), Coordinate3D(-y, -x, -z))

    val zeroDistance: Int = Math.sqrt(x * x + y * y + z * z).toInt

    def distanceTo(that: Coordinate3D): Int =
      Math.sqrt(Math.pow(x - that.x, 2) + Math.pow(y - that.y, 2) + Math.pow(z - that.z, 2)).toInt

    def manhattanDistance(other: Coordinate3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

    def +(other: Coordinate3D): Coordinate3D = Coordinate3D(x + other.x, y + other.y, z + other.z)

    def -(other: Coordinate3D): Coordinate3D = Coordinate3D(x - other.x, y - other.y, z - other.z)

    override def compare(that: Coordinate3D): Int = zeroDistance.compare(that.zeroDistance)
  }

  case class Scanner(name: String, coordinate: Option[Coordinate3D], beacons: Seq[Coordinate3D]) {
    val distancesPerBeacon: Map[Coordinate3D, List[(Coordinate3D, Int)]] = beacons.map(d => (d -> beacons.toList.map(b => (b, b.distanceTo(d))).sortBy(_._2))).toMap
  }

  object Scanner {
    def apply(input: String): Scanner = {
      val lines = input.split("\n")
      val name = lines.head match {
        case s"--- $name ---" => name
      }
      val coordindates = lines.tail.map {
        case s"$x,$y,$z" => Coordinate3D(x.toInt, y.toInt, z.toInt)
      }.toSeq

      Scanner(name, None, coordindates)
    }
  }

}
