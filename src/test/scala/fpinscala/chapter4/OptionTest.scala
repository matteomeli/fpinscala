import org.scalatest._

import fpinscala.chapter4.{Option, Some, None}

class OptionTest extends FlatSpec with Matchers {
  "Function map" should "apply the given function if the Option is not None" in {
    val s = Some(0)
    val n: Option[Int] = None
    s map(_ + 1) should be (Some(1))
    n map(_ + 1) should be (None)
  }

  "Function getOrElse" should "return a default value is an Option is None" in {
    None getOrElse(2) should be (2)
    Some(1) getOrElse(2) should be (1)
  }

  // TODO: Implement tests for basic Option's functions

  // TODO: Test variance function
}
