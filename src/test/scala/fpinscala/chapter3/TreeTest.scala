import org.scalatest._

import fpinscala.chapter3.{Tree, Branch, Leaf}
import Tree._

class TreeTest extends FlatSpec with Matchers {
  "Function size" should "return the number of nodes in a tree" in {
    Tree.size(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))) should be (7)
  }

  "Function max" should "return the maximum element in a tree" in {
    maximum(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))) should be (3)
  }

  "Function depth" should "return the maximum path length from the root to any leaf" in {
    depth(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))) should be (2)
  }

  "Function map" should "modify each element mantaining the tree structure" in {
    map(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3))))(_.toString) should be (Branch(Branch(Leaf("0"), Leaf("1")), Branch(Leaf("2"), Leaf("3"))))
  }
}
