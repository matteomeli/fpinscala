import org.scalatest._

import fpinscala.chapter2.GettingStarted._

class GettingStartedTest extends FlatSpec with Matchers {
  "Function fibonacci" should "return the number at the requested position in the fibonacci sequence" in {
    fibonacci(0) should be (0)
    fibonacci(1) should be (1)
    fibonacci(2) should be (1)
    fibonacci(3) should be (2)
    fibonacci(4) should be (3)
    fibonacci(5) should be (5)
    fibonacci(6) should be (8)
  }

  "Function isSorted" should "return true if a list is sorted" in {
    val gt = (x: Int, y: Int) => x > y
    isSorted(Array(1, 2, 3, 4, 5), gt) should be (true)
    isSorted(Array(5, 4, 3, 2, 1), gt) should be (false)
    isSorted(Array(1, 2, 3, 5, 4), gt) should be (false)
    isSorted(Array(1, 1, 2, 2, 3), gt) should be (true)
    isSorted(Array(1, 1, 1, 1, 1), gt) should be (true)
  }
}
