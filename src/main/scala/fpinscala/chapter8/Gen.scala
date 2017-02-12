package fpinscala.chapter8

import fpinscala.chapter6.{RNG, State}

trait Prop {
  def check: Boolean

  // Exercise 8.3
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}

object Prop {
  def forAll[A](ga: Gen[A])(pf: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A])

object Gen {
  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // Exercise 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val first = State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
    val second = State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
    Gen(first.map2(second)((_, _)))
  }
}