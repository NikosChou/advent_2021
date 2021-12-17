package de.choustoulakis.advent2021.day16

import de.choustoulakis.advent2021.Puzzle
import de.choustoulakis.advent2021.day16.Day16.*

import java.math.BigInteger

trait Day16 extends Puzzle[String, Output] :
  val day = 16

  override def solve(input: String): Output =
    val operatorPacket = Packet(input)._1.asInstanceOf[OperatorPacket]
    val part1 = operatorPacket.allPackets.map(_.version).map(Integer.parseInt(_, 2)).sum

    Output(part1, operatorPacket.value)


object Day16:
  case class Output(part1: Int, part2: Long)

  val toBinaryMap = (0 until 16).map(i => (i.toHexString.head.toUpper -> Integer.toBinaryString(i))).map(t => t.copy(_2 = "0" * (4 - t._2.length) + t._2)).toMap

  sealed trait Packet:
    def toBinary: String

    def version: String

    def typeId: String

    def value: Long

  object Packet:
    def apply(input: String): (Packet, String) = {
      val binary = if (input.filterNot(_ == '0').filterNot(_ == '1').isEmpty) input else input.map(toBinaryMap).mkString
      if (input.distinct == "0" || input.isEmpty) return (EmptyPacket, "")
      val version = binary.take(3)
      val typeId = binary.drop(3).take(3)
      val packet = typeId match {
        case s"100" => {
          val groups = binary.drop(6).grouped(5).takeWhile(_.head == '1').toSeq :+ binary.drop(6).grouped(5).find(_.head == '0').get
          LiteralPacket(version, typeId, groups)
        }
        case _ => {
          val lengthTypeId = binary(6)
          if (lengthTypeId == '1') {
            val totalLength = binary.drop(7).take(11)
            val length = Integer.parseInt(totalLength, 2)
            val packets = binary.drop(7 + 11)
            val step = packets.length / length
            if(typeId == "110") {
              println(binary)
            }//000 110 1 00000000010 [[000 100 11110 11110 01101] [001 100 11100 11100 11000 11111 11010 00011] [000 100 10010 10011 10000 01110]]
            val packetList = packets.sliding(step, step).foldLeft((Seq[Packet](), "")) { case (prev, p) =>
              val (pack, rest) = Packet(prev._2 + p)
              (prev._1.:+(pack), rest)
            }._1.take(length)

            OperatorPacket(version, typeId, lengthTypeId, totalLength, packetList)
          } else {
            val bitsLength = binary.drop(7).take(15)
            val length = Integer.parseInt(bitsLength, 2)
            val packets = binary.drop(7 + 15).take(length)

            def helper(str: String): Seq[Packet] = {
              if (str.isEmpty) Seq()
              else {
                val (p, rest) = Packet(str)
                helper(rest).+:(p)
              }
            }

            val packetList = helper(packets)
            OperatorPacket(version, typeId, lengthTypeId, bitsLength, packetList)
          }
        }
      }
      (packet, binary.replaceFirst(packet.toBinary, ""))
    }

  case object EmptyPacket extends Packet {
    def toBinary: String = ???

    def version: String = ???

    def typeId: String = ???

    def value = ???
  }

  case class OperatorPacket(version: String, typeId: String, lengthTypeId: Char, totalLength: String, private val packets: Seq[Packet]) extends Packet {
    val normalizePackets = packets.filterNot(_ == EmptyPacket)
    override val toBinary = version + typeId + lengthTypeId + totalLength + normalizePackets.map(_.toBinary).mkString

    lazy val allPackets = {
      def helper(packet: Packet): Seq[Packet] = {
        if (packet.isInstanceOf[LiteralPacket]) Seq(packet)
        else if (packet == EmptyPacket) Seq(packet)
        else packet.asInstanceOf[OperatorPacket].normalizePackets.flatMap(helper) :+ packet
      }

      helper(this)
    }

    def value = {
      Integer.parseInt(typeId, 2) match {
        case 0 => normalizePackets.map(_.value).sum
        case 1 => normalizePackets.map(_.value).product
        case 2 => normalizePackets.map(_.value).min
        case 3 => normalizePackets.map(_.value).max
        case 5 =>
          assert(normalizePackets.size == 2, s"Size was ${normalizePackets.size} $this")
          if (normalizePackets.head.value > normalizePackets.last.value) 1 else 0
        case 6 =>
          assert(normalizePackets.size == 2, s"Size was ${normalizePackets.size} $this")
          if (normalizePackets.head.value < normalizePackets.last.value) 1 else 0
        case 7 =>
          assert(normalizePackets.size == 2, s"Size was ${normalizePackets.size} $this")
          if (normalizePackets.head.value == normalizePackets.last.value) 1 else 0
      }
    }
  }

  case class LiteralPacket(version: String, typeId: String, groups: Seq[String]) extends Packet {
    override val toBinary = version + typeId + groups.mkString
    assert(typeId == "100", s"TypeId was $typeId")

    def value = {
      val res = groups.map(_.tail).mkString
      new BigInteger(res, 2).longValue()
    }
  }
