import org.scalatest._

import fpinscala.chapter3.List._

class ListTest extends FlatSpec with Matchers {
  "Function tail" should "return the tail of a list" in {
    tail(fpinscala.chapter3.List(1, 2, 3, 4)) should be (fpinscala.chapter3.List(2, 3, 4))

    assertThrows [NoSuchElementException] {
      tail(fpinscala.chapter3.List())
    }
  }

  "Function setHead" should "return a new list with the updated head" in {
    setHead(fpinscala.chapter3.List(1, 2, 3, 4, 5), 0) should be (fpinscala.chapter3.List(0, 2, 3, 4, 5))

    assertThrows [NoSuchElementException] {
      setHead(fpinscala.chapter3.List(), 1)
    }
  }

  "Function drop" should "return a list without the first n elements" in {
    drop(fpinscala.chapter3.List(1, 2, 3, 4, 5), 2) should be (fpinscala.chapter3.List(3, 4, 5))

    drop(fpinscala.chapter3.List(1), 1) should be (fpinscala.chapter3.List())

    drop(fpinscala.chapter3.List(1, 2, 3, 4, 5), 6) should be (fpinscala.chapter3.List())
  }

  "Function dropWhile" should "return a list without the first elements that match the given predicate" in {
    val lessThanFour = (x: Int) => x < 4
    dropWhile(fpinscala.chapter3.List(1, 2, 3, 4, 5), lessThanFour) should be (fpinscala.chapter3.List(4, 5))

    val greaterThanTen = (x: Int) => x > 10
    dropWhile(fpinscala.chapter3.List(1, 2, 3, 4, 5), greaterThanTen) should be (fpinscala.chapter3.List(1, 2, 3, 4, 5))

    val lessOrEqualThanFive = (x: Int) => x <= 5
    dropWhile(fpinscala.chapter3.List(1, 2, 3, 4, 5), lessOrEqualThanFive) should be (fpinscala.chapter3.List())
  }

  "Function init" should "return a list with all its elements but the last" in {
    init(fpinscala.chapter3.List(1, 2, 3, 4, 5)) should be (fpinscala.chapter3.List(1, 2, 3, 4))

    init(fpinscala.chapter3.List(1)) should be (fpinscala.chapter3.List())

    assertThrows [NoSuchElementException] {
      init(fpinscala.chapter3.List())
    }
  }
}
