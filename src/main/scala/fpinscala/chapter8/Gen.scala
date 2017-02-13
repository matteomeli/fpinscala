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

case class Gen[A](sample: State[RNG, A]) {
  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // This is actually the equivalent of the above ^
  def flatMap2[B](f: A => Gen[B]): Gen[B] =
  Gen(State(rng => {
    val (a, rng2) = sample.run(rng)
    f(a).sample.run(rng2)
  }))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => this.listOfN(n))
}

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

  def packOption[A](g: Gen[A]): Gen[Option[A]] = Gen(g.sample.map(Some(_)))
  def unpackOption[A](g: Gen[Option[A]]): Gen[A] = ???

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double)).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }
}