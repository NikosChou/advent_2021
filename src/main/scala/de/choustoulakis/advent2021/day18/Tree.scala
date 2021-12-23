package de.choustoulakis.advent2021.day18

import java.math.MathContext
import scala.math.BigDecimal.RoundingMode

sealed trait Tree {
  this: Tree =>
  def addL(t: Tree): Tree

  def addR(t: Tree): Tree

  def +(that: Tree): Tree

  def normalizeStep: Tree

  def magnitude: Long

  def depth: Int = {
    def go(t: Tree, depth: Int = 0): Int = {
      t match {
        case Node(left, right) => List(go(left, depth + 1), go(right, depth + 1)).max
        case _ => depth
      }
    }

    go(this)
  }

  def printPreOrder: String = {
    def go(t: Tree): String = t match {
      case Leaf(v) => s"$v"
      case Node(left, right) => s"[${go(left)},${go(right)}]"
      case _ => ""
    }

    go(this)
  }

  def normalize: Tree = {
    def loop(t: Tree): Tree = {
      val normalized = t.normalizeStep
      if (normalized == t) t else loop(normalized)
    }

    loop(this)
  }
}

object Tree {
  given Conversion[Int, Tree] = Leaf(_)

  def parseTree(in: String): Tree = {
    def findPairs(input: String, acc: Tree): (Tree, String) = {
      if (input.isEmpty) {
        (acc, "")
      } else if (input.startsWith("[")) {
        val (tree, rest) = findPairs(input.tail, EmptyLeaf)
        findPairs(rest, acc.addL(tree))
      } else if (input.startsWith(",")) {
        val (tree, rest) = findPairs(input.tail, EmptyLeaf)
        findPairs(rest, acc.addR(tree))
      } else if (input.startsWith("]")) {
        (acc, input.tail)
      } else {
        (Leaf(input.head.toString.toInt), input.tail)
      }
    }

    findPairs(in, EmptyLeaf)._1
  }
}

case object EmptyLeaf extends Tree {
  override def addL(t: Tree): Tree = t

  override def addR(t: Tree): Tree = t

  override def +(that: Tree): Tree = that

  override def normalizeStep: Tree = this

  override def magnitude: Long = 0L
}

case class Leaf(var value: Int) extends Tree {
  override def addL(t: Tree): Tree = Node(t, this)

  override def addR(t: Tree): Tree = Node(this, t)

  override def +(that: Tree): Tree = Node(this, that)

  override def normalizeStep: Tree = this

  override def magnitude: Long = value

}

case class Node(left: Tree, right: Tree) extends Tree {
  override def addL(t: Tree): Tree = Node(t, this)

  override def addR(t: Tree): Tree = Node(this, t)

  override def +(that: Tree): Tree = Node(this, that)

  override def magnitude: Long = (3 * left.magnitude) + (2 * right.magnitude)

  override def normalizeStep: Tree = {
    def updateLeafLeft(tree: Tree, toAdd: Int): Tree = {
      tree match {
        case Leaf(v) => Leaf(v + toAdd)
        case Node(left, right) => Node(updateLeafLeft(left, toAdd), right)
      }
    }

    def updateLeafRight(tree: Tree, toAdd: Int): Tree = {
      tree match {
        case Leaf(v) => Leaf(v + toAdd)
        case Node(left, right) => Node(left, updateLeafRight(right, toAdd))
      }
    }

    def explode(tree: Tree, d: Int): (Tree, Int, Int) = {
      tree match {
        case Node(Leaf(left), Leaf(right)) if (d > 3) => (EmptyLeaf, left, right)
        case Node(left, right) => {
          val (leftTree, ll, lr) = explode(left, d + 1)
          val (rghtTree, rl, rr) = if (leftTree == left) explode(right, d + 1) else (right, 0, 0)

          if (leftTree == left && rghtTree == right) {
            (tree, 0, 0)
          } else if (leftTree == EmptyLeaf) {
            (Node(0, updateLeafLeft(right, lr)), ll, 0)

          } else if (rghtTree == EmptyLeaf) {
            (Node(updateLeafRight(left, rl), 0), 0, rr)
          } else {
            if (leftTree == left) {
              (Node(updateLeafRight(leftTree, ll + rl), rghtTree), 0, lr + rr)
            } else {
              (Node(leftTree, updateLeafLeft(rghtTree, lr + rr)), rl + ll, 0)
            }
          }
        }
        case _ => (tree, 0, 0)
      }
    }

    def split(tree: Tree): Tree = {
      tree match {
        case Leaf(value) if value > 9 =>
          val res = BigDecimal(value)./(BigDecimal(2))
          Node(res.setScale(0, RoundingMode.DOWN).toInt, res.setScale(0, RoundingMode.UP).toInt)
        case Node(left, right) =>
          val resLeft = split(left)
          if (resLeft != left) Node(resLeft, right) else Node(left, split(right))
        case _: Leaf => tree
      }
    }

    def splitPos(tree: Tree): (Int, Boolean) = {
      def loop(t: Tree, pos: Int): (Int, Boolean) = {
        t match {
          case Leaf(v) if v > 9 => (pos + 1, true)
          case Leaf(v) => (pos + 1, false)
          case Node(left, right) =>
            val (poss, pass) = loop(left, pos)
            if (pass) (poss, true) else loop(right, poss)
        }
      }

      loop(tree, -1)
    }

    def explodePos(tree: Tree): (Int, Int) = {
      def loop(t: Tree, pos: Int, d: Int): (Int, Int) = {
        t match {
          case Node(left, right) =>
            val (poss, dd) = loop(left, pos, d + 1)
            if (dd > 3) (poss, dd) else loop(right, poss, d + 1)
          case _ => (pos + 1, d)
        }
      }

      loop(tree, -1, -1)
    }

    val splitPosition = splitPos(this)
    val explodePoss = explodePos(this)
    //println(s"$splitPosition    $explodePoss    $printPreOrder")
    if (explodePoss._2 < 4) {
      if (splitPosition._2) {
        return split(this)
      }
    }
    explode(this, 0)._1
  }
}
