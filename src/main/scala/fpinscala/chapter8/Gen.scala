package fpinscala.chapter8

import fpinscala.chapter5.Stream
import fpinscala.chapter6.{RNG, State}
import Prop._

sealed trait Result {
  def hasFailed: Boolean
}

case object Passed extends Result {
  def hasFailed = false
}

case class Failed(failure: FailedCase, successess: SuccessCount) extends Result {
  def hasFailed = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // Does not owrk with successive refinement of Prop
  //def check: Boolean

  // Exercise 8.3
  // Does not owrk with successive refinement of Prop
  //def &&(p: Prop): Prop = new Prop {
  //  def check = Prop.this.check && p.check
  //}

  // Exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Failed(msg, _) => p.tag(msg).run(m, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Failed(e, c) => Failed(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](ga: Gen[A])(pf: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(ga)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (pf(a)) Passed else Failed(a.toString, i)
      } catch { case e: Exception => Failed(buildMsg(a, e), i) }
    }.find(_.hasFailed).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) - 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Failed(msg, n) => println(s"! Failed after $n passed test(s):\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
    }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

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

  // Exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

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

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen { g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n => g(n) flatMap { f(_).g(n) } }
}

object SGen {
  // Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { g.listOfN(_) }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {n => g.listOfN(n max 1) }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val maxProp1 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
}
