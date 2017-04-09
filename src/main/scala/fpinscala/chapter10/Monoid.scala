package fpinscala.chapter10

import fpinscala.chapter8.{Gen, Prop}
import Prop._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  // Exercise 10.1
  def intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  def intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A = m.zero
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def secondOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = identity[A]
  }

  def dualEndoMonoid[A]: Monoid[A => A] = dual(endoMonoid)  // This is equivalent to the original endoMonoid but like if op was implemented as `a1 andThen a2`
}

object MonoidLaws {
  import Monoid._

  // Exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a)) && // Identity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)) { case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) } // Associativity
  }

  def concatenate[A](as: List[A])(m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dualEndoMonoid[B])(a => b => f(b, a))(z)

  def main(args: Array[String]): Unit = {
    // Test some of the monoids...
    run(monoidLaws(stringMonoid, Gen.stringN(5)))
    run(monoidLaws(listMonoid[Int], Gen.listOfN(10, Gen.choose(0, 10))))
    run(monoidLaws(intAddition, Gen.choose(0, 10)))
    run(monoidLaws(intMultiplication, Gen.choose(0, 10)))
  }
}