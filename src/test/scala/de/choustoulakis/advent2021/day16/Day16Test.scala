package de.choustoulakis.advent2021.day16

import de.choustoulakis.advent2021.SubmarineSpec

class Day16Test extends SubmarineSpec with Day16 {

  "The day16" should {
    "solve part1_1" in {
      solve("8A004A801A8002F478").part1 shouldBe 16
    }
    "solve part1_2" in {
      solve("620080001611562C8802118E34").part1 shouldBe 12
    }
    "solve part1_3" in {
      solve("C0015000016115A2E0802F182340").part1 shouldBe 23
    }
    "solve part1_4" in {
      solve("A0016C880162017C3686B18A3D4780").part1 shouldBe 31
    }
    // part2
    "solve part2_1" in {
      solve("C200B40A82").part2 shouldBe 3
    }
    "solve part2_2" in {
      solve("04005AC33890").part2 shouldBe 54
    }
    "solve part2_3" in {
      solve("880086C3E88112").part2 shouldBe 7
    }
    "solve part2_4" in {
      solve("CE00C43D881120").part2 shouldBe 9
    }
    "solve part2_5" in {
      solve("D8005AC2A8F0").part2 shouldBe 1
    }
    "solve part2_6" in {
      solve("F600BC2D8F").part2 shouldBe 0
    }
    "solve part2_7" in {
      solve("9C005AC2F8F0").part2 shouldBe 0
    }
    "solve part2_8" in {
      solve("9C0141080250320F1802104A08").part2 shouldBe 1
    }
  }

  "The day 16 Resource" should {
    "solve part 1" in {
      solve(resource.head).part1 shouldBe 860
    }
    "solve part 2" in {
      solve(resource.head).part2 shouldBe 470949537659L
    }
  }

}
