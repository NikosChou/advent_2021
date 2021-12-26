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
      result.normalize.printPreOrder shouldBe "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
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
      val res = Tree.parseTree("[[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]")
      res.normalize.printPreOrder shouldBe "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    }
    "add 6 numbers" in {
      val res = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]".split("\n").map(_.trim).map(Tree.parseTree).reduce(_ + _)
      res.normalize.printPreOrder shouldBe "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    }
    "large test" in {
      val input: String =
        """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
          |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
          |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
          |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
          |[7,[5,[[3,8],[1,4]]]]
          |[[2,[2,2]],[8,[8,1]]]
          |[2,9]
          |[1,[[[9,3],9],[[9,0],[0,7]]]]
          |[[[5,[7,4]],7],1]
          |[[[[4,2],2],6],[8,7]]""".stripMargin

      val res = input.split("\n").map(Tree.parseTree(_)).foldLeft(EmptyLeaf.asInstanceOf[Tree]){(l, r) =>
        println("  " + l.printPreOrder)
        println("+ " + r.printPreOrder)
        val normalize = (l + r).normalize
        println("= " + normalize.printPreOrder)
        println()
        normalize
      }
      res.printPreOrder shouldBe "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    }
    "the magnidude" in {
      Tree.parseTree("[[1,2],[[3,4],5]]").magnitude shouldBe 143
      Tree.parseTree("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude shouldBe 3488
    }
  }

  "The day 18 Resource" should {
    val result = solve(resource)
    "solve part 1" in {
      result.part1 shouldBe 3051
    }
    "solve part 2" in {
      result.part2 shouldBe 4812
    }
  }

}
