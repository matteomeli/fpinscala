package fpinscala.chapter10

import fpinscala.chapter8.{Gen, Prop}
import Prop._

import language.higherKinds

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

  // Exercise 10.10
  sealed trait WordCount
  case class Stub(chars: String) extends WordCount
  case class Part(lStub: String, words: Int, rStub: String) extends WordCount

  val wcMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    def op(w1: WordCount, w2: WordCount): WordCount = {
      (w1, w2) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
        case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => 
          Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      }
    }
    def zero: WordCount = Stub("")
  }

  // Exercise 10.11
  def wordCount(s: String): Int = {
    foldMapV(s.toIndexedSeq, wcMonoid) { c =>
        if (c.isWhitespace) Part("", 0, "")
        else Stub(c.toString)
      } match {
        case Stub(s) => s.length min 1
        case Part(l, w, r) => l.length min 1 + w + r.length min 1
    }
  }

  // Exercise 10.12
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dualEndoMonoid[B])(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
      //foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    // Exercise 10.15
    def toList[A](fa: F[A]): List[A] =
      //foldMap(fa)(List(_))(listMonoid[A])
      foldRight(fa)(List[A]())(_ :: _)
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  // Exercise 10.13
  import fpinscala.chapter3._
  import fpinscala.chapter3.Tree._
  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    // No need to use mb.zero in the definition
    // Some types are foldable with something smaller than a monoid
    // A `monoid` without zero is a `semigroup`
    // Using fold defined in Tree companion object
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      fold(as)(f)(mb.op(_, _))
  }

  // Exercise 10.14
  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](oa: Option[A])(z: B)(f: (A, B) => B): B =
      oa.map(f(_, z)).getOrElse(z)

    override def foldLeft[A, B](oa: Option[A])(z: B)(f: (B, A) => B): B =
      oa.map(f(z, _)).getOrElse(z)

    override def foldMap[A, B](oa: Option[A])(f: A => B)(mb: Monoid[B]): B =
      oa.map(f).getOrElse(mb.zero)
  }

  // Exercise 10.16
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))
    def zero: (A, B) = (ma.zero, mb.zero)
  }

  // Exercise 10.17
  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B = a => m.op(f1(a), f2(a))
    def zero: A => B = a => m.zero
  }

  // Exercise 10.18
  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, mv.op(a.getOrElse(k, mv.zero), b.getOrElse(k, mv.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(M)
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