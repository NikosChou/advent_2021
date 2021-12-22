package de.choustoulakis.advent2021.day18

import de.choustoulakis.advent2021.SubmarineSpec
import de.choustoulakis.advent2021.day18.Day18.*

class Day18Test extends SubmarineSpec with Day18 {

  "The Pre puzzle code" should {
    "Print the tree as snail number" in {
      val snailNumber = Node(Node(Node(Node(1, 2), Node(3, 4)), Node(Node(5, 6), Node(7, 8))), 9).printPreOrder
      snailNumber shouldBe "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
    }
    "Read snail number as Tree" in {
      val tree = Tree.parseTree("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")
      tree shouldBe Node(Node(Node(Node(Leaf(1), Leaf(3)), Node(Leaf(5), Leaf(3))), Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(7)))), Node(Node(Node(Leaf(4), Leaf(9)), Node(Leaf(6), Leaf(9))), Node(Node(Leaf(8), Leaf(2)), Node(Leaf(7), Leaf(3)))))
    }
    "add 2 numbers" in {
      val result = Node(Node(3, 4), 5) + Node(1, 2)
      result shouldBe Node(Node(Node(3, 4), 5), Node(1, 2))
    }
    "normalize number (4 pairs) left" in {
      val tree = Tree.parseTree("[[[[[9,8],1],2],3],4]").normalizeStep
      tree.printPreOrder shouldBe "[[[[0,9],2],3],4]"
    }
    "normalize number (4 pairs) right" in {
      val tree = Tree.parseTree("[7,[6,[5,[4,[3,2]]]]]").normalizeStep
      tree.printPreOrder shouldBe "[7,[6,[5,[7,0]]]]"
    }
    "normalize number (4 pairs) middle" in {
      val tree = Tree.parseTree("[[6,[5,[4,[3,2]]]],1]").normalizeStep
      tree.printPreOrder shouldBe "[[6,[5,[7,0]]],3]"
    }
    "normalize recursive steps" in {
      val tree = Tree.parseTree("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").normalizeStep
      tree.printPreOrder shouldBe ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      tree.normalizeStep.printPreOrder shouldBe ("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    }
    "addition explode split repeat steps" in {
      import Tree.{given}

      val first = Tree.parseTree("[[[[4,3],4],4],[7,[[8,4],9]]]")
      val result = first + Node(1, 1)
      result.printPreOrder shouldBe "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
      result.normalizeStep.printPreOrder shouldBe "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
      result.normalizeStep.normalizeStep.printPreOrder shouldBe "[[[[0,7],4],[15,[0,13]]],[1,1]]"
      result.normalizeStep.normalizeStep.normalizeStep.printPreOrder shouldBe "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
      result.normalizeStep.normalizeStep.normalizeStep.normalizeStep.printPreOrder shouldBe "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
      result.normalizeStep.normalizeStep.normalizeStep.normalizeStep.normalizeStep.printPreOrder shouldBe "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    }
    "addition explode split repeat" in {
      val result = Tree.parseTree("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
      result.normalize.printPreOrder shouldBe "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    }
    "add 4 numbers" in {
      val res = "[1,1]\n      [2,2]\n      [3,3]\n      [4,4]".split("\n").map(_.trim).map(Tree.parseTree).reduce(_ + _)
      res.normalize.printPreOrder shouldBe "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    }
    "add 5 numbers" in {
      val res = Tree.parseTree("[[[[[[1,1],[2,2]],[3],3]],[4,4]],[5,5]]")
      res.normalize.printPreOrder shouldBe "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    }
  }
  "The day17" ignore {
    val result = solve("target area: x=20..30, y=-10..-5")
    "solve part1" in {
      result.part1 shouldBe 45
    }
    "solve part2" in {
      result.part2 shouldBe 112
    }
  }

  "The day 17 Resource" ignore {
    val result = solve(resource.head)
    "solve part 1" in {
      result.part1 shouldBe 14535
    }
    "solve part 2" in {
      result.part2 shouldBe 2270
    }
  }

}
