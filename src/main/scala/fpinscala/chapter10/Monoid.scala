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

  // Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  // Exercise 10.8
  import fpinscala.chapter7.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p1: Par[A], p2: Par[A]): Par[A] = map2(p1, p2)(m.op)
    def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    flatMap(parMap(v)(f)) { bs =>
      foldMapV(bs, par(m))(b => lazyUnit(b))
    }
  }

  // Exercise 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[((Int, Int, Boolean), (Int, Int, Boolean))]] {
      def op(o1: Option[((Int, Int, Boolean), (Int, Int, Boolean))], o2: Option[((Int, Int, Boolean), (Int, Int, Boolean))]):
      Option[((Int, Int, Boolean), (Int, Int, Boolean))] = {
        (o1, o2) match {
          case (Some(((x1, y1, p), (u1, v1, r))), Some(((x2, y2, q), (u2, v2, s)))) => 
            Some(((x1 min x2, y1 max y2, p && q && y1 <= x2), (u1 max u2, v1 min v2, r && s && v1 >= u2)))
          case (x, None) => x
          case (None, x) => x
        }
      }
      def zero: Option[((Int, Int, Boolean), (Int, Int, Boolean))] = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true), (i, i, true))).map { case (asc, desc) => asc._3 || desc._3 }.getOrElse(true)
  }

  def main(args: Array[String]): Unit = {
    // Test some of the monoids...
    Prop.run(monoidLaws(stringMonoid, Gen.stringN(5)))
    Prop.run(monoidLaws(listMonoid[Int], Gen.listOfN(10, Gen.choose(0, 10))))
    Prop.run(monoidLaws(intAddition, Gen.choose(0, 10)))
    Prop.run(monoidLaws(intMultiplication, Gen.choose(0, 10)))

    println(ordered(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))) // == true (ascending)
    println(ordered(Vector(9, 8, 7, 6, 5, 4, 3, 2, 1, 0))) // == true (descending)
    println(ordered(Vector(1, 1, 1)))    // == true
    println(ordered(Vector(1, 1, 2, 2))) // == true
    println(ordered(Vector(1, 3, 2, 4))) // == false
  }
}