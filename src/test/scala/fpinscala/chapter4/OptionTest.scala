import org.scalatest._

import fpinscala.chapter4.{Option, Some, None}

class OptionTest extends FlatSpec with Matchers {
  "Function map" should "apply the given function if the Option is not None" in {
    val s = Some(0)
    val n: Option[Int] = None
    s map(_ + 1) should be (Some(1))
    n map(_ + 1) should be (None)
  }

  // TODO: Implement tests for basic Option's functions

  // TODO: Test variance function
}
