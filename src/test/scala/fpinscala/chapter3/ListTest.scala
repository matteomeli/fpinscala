import org.scalatest._

import fpinscala.chapter3.List
import List._

class ListTest extends FlatSpec with Matchers {
  "Function tail" should "return the tail of a list" in {
    tail(List(1, 2, 3, 4)) should be (List(2, 3, 4))

    assertThrows [NoSuchElementException] {
      tail(List())
    }
  }

  "Function setHead" should "return a new list with the updated head" in {
    setHead(List(1, 2, 3, 4, 5), 0) should be (List(0, 2, 3, 4, 5))

    assertThrows [NoSuchElementException] {
      setHead(List(), 1)
    }
  }

  "Function drop" should "return a list without the first n elements" in {
    drop(List(1, 2, 3, 4, 5), 2) should be (List(3, 4, 5))

    drop(List(1), 1) should be (List())

    drop(List(1, 2, 3, 4, 5), 6) should be (List())
  }

  "Function dropWhile" should "return a list without the first elements that match the given predicate" in {
    val lessThanFour = (x: Int) => x < 4
    dropWhile(List(1, 2, 3, 4, 5), lessThanFour) should be (List(4, 5))

    val greaterThanTen = (x: Int) => x > 10
    dropWhile(List(1, 2, 3, 4, 5), greaterThanTen) should be (List(1, 2, 3, 4, 5))

    val lessOrEqualThanFive = (x: Int) => x <= 5
    dropWhile(List(1, 2, 3, 4, 5), lessOrEqualThanFive) should be (List())
  }

  "Function init" should "return a list with all its elements but the last" in {
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
}
