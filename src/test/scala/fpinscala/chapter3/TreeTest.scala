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

  "Function fold" should "generalise other functions on trees" in {
    fold(Branch(Leaf(1), Leaf(2)))(_ => 1)(1 + _ + _) should be (Tree.size(Branch(Leaf(1), Leaf(2))))

    fold(Branch(Leaf(1), Leaf(2)))(identity)(_ max _) should be (maximum(Branch(Leaf(1), Leaf(2))))

    fold(Branch(Leaf(1), Leaf(2)))(_ => 0)((x, y) => 1 + x max y) should be (depth(Branch(Leaf(1), Leaf(2))))

    mapViaFold(Branch(Leaf(1), Leaf(2)))(_.toString) should be (map(Branch(Leaf(1), Leaf(2)))(_.toString))
  }
}
