import org.scalatest._

import fpinscala.chapter3.{List, Nil}
import List._

class ListTest extends FlatSpec with Matchers {
  "Function tail" should "return the tail of a list" in {
    tail(List(1, 2, 3, 4)) should be (List(2, 3, 4))

    assertThrows [NoSuchElementException] {
      tail(List())
    }
  }

  "Function setHead" should "return a new list Via the updated head" in {
    setHead(List(1, 2, 3, 4, 5), 0) should be (List(0, 2, 3, 4, 5))

    assertThrows [NoSuchElementException] {
      setHead(List(), 1)
    }
  }

  "Function drop" should "return a list Viaout the first n elements" in {
    drop(List(1, 2, 3, 4, 5), 2) should be (List(3, 4, 5))

    drop(List(1), 1) should be (List())

    drop(List(1, 2, 3, 4, 5), 6) should be (List())
  }

  "Function dropWhile" should "return a list Viaout the first elements that match the given predicate" in {
    val lessThanFour = (x: Int) => x < 4
    dropWhile(List(1, 2, 3, 4, 5), lessThanFour) should be (List(4, 5))

    val greaterThanTen = (x: Int) => x > 10
    dropWhile(List(1, 2, 3, 4, 5), greaterThanTen) should be (List(1, 2, 3, 4, 5))

    val lessOrEqualThanFive = (x: Int) => x <= 5
    dropWhile(List(1, 2, 3, 4, 5), lessOrEqualThanFive) should be (List())
  }

  "Function init" should "return a list Via all its elements but the last" in {
    init(List(1, 2, 3, 4, 5)) should be (List(1, 2, 3, 4))

    init(List(1)) should be (List())

    assertThrows [NoSuchElementException] {
      init(List())
    }
  }

  "Function length" should "calculate the length of a list" in {
    List.length(List(1, 2, 3, 4, 5)) should be (5)
  }

  "Function foldLeft" should "be equivalent to foldRight" in {
    foldLeft(List(1, 2, 3, 4, 5), 0)((x, y) => x + y) should be (foldRight(List(1, 2, 3, 4, 5), 0)((x, y) => x + y))
  }

  "Function sum2" should "be euivalent to sum" in {
    sum2(List(1, 2, 3, 4, 5)) should be (sum(List(1, 2, 3, 4, 5)))
  }

  "Function product2" should "be euivalent to product" in {
    product2(List(1, 2, 3, 4, 5)) should be (product(List(1, 2, 3, 4, 5)))
  }

  "Function length2" should "be equivalent to length" in {
    length2(List(1, 2, 3, 4, 5)) should be (List.length(List(1, 2, 3, 4, 5)))
  }

  "Function reverse" should "return the reverse of a list" in {
    reverse(List(1, 2, 3, 4, 5)) should be (List(5, 4, 3, 2, 1))
  }

  "Function foldRightViaFoldLeft" should "return the same result of foldRight" in {
    foldRightViaFoldLeft(List(1, 2, 3), 0)((x, y) => x + y) should be (foldRight(List(1, 2, 3), 0)((x, y) => x + y))
  }

  "Function append" should "return a list of elements plus the new one appended at the end" in {
    append(List(1, 2, 3), 4) should be (List(1, 2, 3, 4))
    appendViaFoldRight(List(1, 2, 3), List(4)) should be (append(List(1, 2, 3), 4))
    appendViaFoldLeft(List(1, 2, 3), List(4)) should be (appendViaFoldRight(List(1, 2, 3), List(4)))
  }

  "Function flatten" should "concatenate a list of lists in a single list" in {
    flatten(List(List(1, 2), List(3, 4), List(5, 6))) should be (List(1, 2, 3, 4, 5, 6))
  }

  "Function addOne" should "add 1 to each element of a list of integers" in {
    addOne(List(1, 2, 3)) should be (List(2, 3, 4))
    addOne(List()) should be (List())
  }

  "Function doublesToStrings" should "transform each value in a list of doubles into a string" in {
    doublesToStrings(List(1.0, 2.0, 3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  "Function map" should "modify each element of a list while mantaining the structure of the list" in {
    val l = List(1.0, 2.0, 3.0)
    map(l)(_.toString) should be (doublesToStrings(l))
    map(l)(_ + 1) should be (List(2.0, 3.0, 4.0))
  }

  "Function filter" should "remove all the elements from a list unless they satisfy the given procedure" in {
    filter(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0) should be (List(0, 2, 4, 6, 8))
  }

  "Function flatMap" should "work like map except the input function returnsa list" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
  }

  "Function filterViaFlatMap" should "behave like filter" in {
    val l = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val even = (n : Int) => n % 2 == 0
    filterViaFlatMap(l)(even) should be (filter(l)(even))
  }

  "Function addPairs" should "take two lists and construct a new list by adding correspondent elements" in {
    addPairs(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
    addPairs(List(1, 2), List(4, 5, 6)) should be (List(5, 7))
  }

  "Function zipWith" should "take two lists and a function and construct a new list by applying the function pairwise" in {
    zipWith(List(4, 2, 5, 6), List(2, 6, 2, 3))(_ + _) should be (List(6, 8, 7, 9))
    zipWith(List(1, 2, 3, 4), List("one", "two", "three", "four"))((_, _)) should be (List((1, "one"), (2, "two"), (3, "three"), (4, "four")))
  }

  "Function hasSubsequence" should "take return true if a list contains another list" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), Nil:List[Int]) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(5, 6)) should be (false)
    hasSubsequence(Nil, List(5, 6)) should be (false)
    hasSubsequence(Nil, Nil) should be (true)
  }
}
