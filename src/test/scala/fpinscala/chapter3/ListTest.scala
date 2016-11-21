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
}
