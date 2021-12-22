package de.choustoulakis.advent2021.day18

import java.math.MathContext
import scala.math.BigDecimal.RoundingMode

sealed trait Tree {
  this: Tree =>
  def addL(t: Tree): Tree

  def addR(t: Tree): Tree

  def +(that: Tree): Tree

  def normalizeStep: Tree

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
    }

    go(this)
  }

  def normalize: Tree = {
    def loop(t: Tree): Tree = {
      val normalized = t.normalizeStep
      if(normalized == t) t else loop(normalized)
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
}

case class Leaf(var value: Int) extends Tree {
  override def addL(t: Tree): Tree = Node(t, this)

  override def addR(t: Tree): Tree = Node(this, t)

  override def +(that: Tree): Tree = Node(this, that)

  override def normalizeStep: Tree = this

}

case class Node(left: Tree, right: Tree) extends Tree {
  override def addL(t: Tree): Tree = Node(t, this)

  override def addR(t: Tree): Tree = Node(this, t)

  override def +(that: Tree): Tree = Node(this, that)

  override def normalizeStep: Tree = {
    def updateLeaf(tree: Tree, toAdd: Int): Tree = {
      tree match {
        case Leaf(v) => Leaf(v + toAdd)
        case Node(left, right) => Node(updateLeaf(left, toAdd), right)
      }
    }

    def explode(tree: Tree, d: Int): (Tree, Int, Int) = {
      tree match {
        case node@Node(Leaf(v), Node(Leaf(left), Leaf(right))) if (d > 2) => (Node(Leaf(left + v), 0), -1, right)
        case node@Node(Node(Leaf(left), Leaf(right)), Leaf(v)) if (d > 2) => (Node(0, Leaf(right + v)), left, -1)
        case Node(left, right) => {
          val (leftTree, ll, lr) = explode(left, d + 1)
          val (rghtTree, rl, rr) = if (left == left && ll == -1 && ll == -1) explode(right, d + 1) else (right, -1, -1)
          if (leftTree != left) {
            if (lr > -1) {
              (Node(leftTree, updateLeaf(right, lr)), -1, -1)
            } else {
              (Node(leftTree, right), -1, -1)
            }
          } else if (rghtTree != right) {
            if (rl > -1) {
              (Node(updateLeaf(left, rl), rghtTree), -1, -1)
            } else {
              (Node(left, rghtTree), rl, rr)
            }
          } else {
            if (lr > -1) {
              (Node(leftTree, updateLeaf(right, lr)), -1, -1)
            } else if (rl > -1) {
              (Node(updateLeaf(left, rl), rghtTree), -1, -1)
            } else {
              (tree, -1, -1)
            }
          }
        }
        case _ => (tree, -1, -1)
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
        case _:Leaf => tree
      }
    }

    val (exploded, _, _) = explode(this, 0)
    if (exploded != this) exploded else split(this)
  }
}
