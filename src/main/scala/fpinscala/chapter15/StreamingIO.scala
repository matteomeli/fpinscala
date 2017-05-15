package fpinscala.chapter15

import fpinscala.chapter13.Free._
import fpinscala.chapter11._
import fpinscala.chapter11.Monad._

object ImperativeIO {
  /**
   * We are going to consider various approaches to the simple task of
   * checking whether a file contains more than 40,000 lines.
   * Our first implementation is an imperative implementation, embedded
   * into `IO`.
   */

  import java.io._

  def linesGt40k(filename: String): IO[Boolean] = IO {
    // There are a number of convenience functions in scala.io.Source
    // for reading from external sources such as files.
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      // Obtain a stateful iterator from the Source
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next // has side effect of advancing to next element
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

                             /*
  The above code is rather low-level, and it's not compositional,
  either. Consider the following scenarios:
  * Check whether the number of _nonempty_ lines in the file exceeds
    40,000
  * Find a line index before 40,000 where the first letter of
    consecutive lines spells out `"abracadabra"`.
  We cannot just compose our existing implementation with some
  other combinator(s) to implement these tasks. Our implementation is
  a monolithic loop, and we must modify this loop directly if we want
  to change its behavior.
  Now imagine if we had a `Stream[String]` for the lines of the file
  and we could assemble functionality using all the `Stream` functions
  we know and love.
                             */

  object Examples {
    val lines: Stream[String] = sys.error("defined elsewhere")
    val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
  }

                            /*
  Could we actually write the above? Not quite. We could 'cheat' and
  return an `IO[Stream[String]]` representing the lines of a file:
                             */

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }
                            /*
  This is called _lazy I/O_, and it's problematic for a number of
  reasons, discussed in the book text. However, it would be nice to
  recover the same high-level, compositional style we are used to
  from our use of `List` and `Stream`.
                             */
}

sealed trait Process[I, O] {
  import Process._

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  // Exercise 15.5
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Emit(h, t) => t |> f(Some(h))
      case Halt() => Halt() |> f(None)
      case Await(g) => Await { g(_) |> p2 }
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await { recv andThen (_ ++ p) }
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await { recv andThen (_.flatMap(f)) }
  }

  // Exercise 15.6
  def zipWithIndex: Process[I, (O, Int)] = {
    def go(count: Int, p: Process[I, O]): Process[I, (O, Int)] = p match {
      case Halt() => Halt()
      case Emit(h, t) => Emit((h, count), go(count + 1, t))
      case Await(recv) => Await { recv andThen (go(count, _)) }
    }
    go(0, this)
  }

  def orElse(p: Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Await(recv) => Await {
      case None => p
      case i => recv(i)
    }
    case _ => this
  }
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = {
      Await {
        case Some(d) => val s = d + acc; Emit(s, go(s))
        case _ => Halt()
      }
    }
    go(0.0)
  }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I,O]()): Process[I, O] = Emit(head, tail)

  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()): Process[I, O] = Await {
    case Some(i) => f(i)
    case _ => fallback
  }

  // Exercise 15.1
  def take[I](n: Int): Process[I, I] = if (n <= 0) Halt() else await { i => emit(i, take(n - 1)) }

  def id[I]: Process[I, I] = lift(identity)

  def drop[I](n: Int): Process[I, I] = if (n <= 0) id else await[I, I] { i => drop[I](n - 1) }

  def takeWhile[I](p: I => Boolean): Process[I, I] = await[I, I] { i =>
    if (p(i)) emit(i)
    else Halt()
  }.repeat

  def takeWhile2[I](p: I => Boolean): Process[I, I] = await[I, I] { i =>
    if (p(i)) emit(i, takeWhile(p))
    else Halt()
  }

  def dropWhile[I](p: I => Boolean): Process[I, I] = await[I, I] { i =>
    if (p(i)) dropWhile(p)
    else emit(i, dropWhile(p))
  }

  def dropWhile2[I](p: I => Boolean): Process[I, I] = await[I, I] { i =>
    if (p(i)) dropWhile(p)
    else emit(i, id)
  }

  // Exercise 15.2
  def count[I]: Process[I, Int] = {
    def go(count: Int): Process[I, Int] = await { _ => val c = count + 1; emit(c, go(c)) }
    go(0)
  }

  // Exercise 15.3
  def mean: Process[Double, Double] = {
    def go(sum: Double, count: Int): Process[Double, Double] = await { d =>
      val c = count + 1
      val s = sum + d
      val m = s / c
      emit(m, go(s, c))
    }
    go(0.0, 0)
  }

  def loop[S, I, O](s: S)(f: (I, S) => (O, S)): Process[I, O] =
    await { i =>
      f(i, s) match {
        case (o, s2) => emit(o, loop(s2)(f))
      }
    }

  // Exercise 15.4
  def sum1: Process[Double, Double] = loop(0.0) { (d, acc) => (d + acc, d + acc) }

  def count1[I]: Process[I, Int] = loop(0) { (_, acc) => (acc + 1, acc + 1) }

  def monad[I]: Monad[({ type f[x] = Process[I, x]})#f] =
    new Monad[({ type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)

      def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p.flatMap(f)
    }

  // Exercise 15.7
  def zip[I, A, B](p1: Process[I, A], p2: Process[I, B]): Process[I, (A, B)] = (p1, p2) match {
    case (Halt(), _) => Halt()
    case (_, Halt()) => Halt()
    case (Emit(h1, t1), Emit(h2, t2)) => Emit((h1, h2), zip(t1, t2))
    case (e @ Emit(_, _), Await(r2)) => Await { i => zip(e, r2(i)) }
    case (Await(r1), e @ Emit(_, _)) => Await { i => zip(r1(i), e) }
    case (Await(r1), Await(r2)) => Await { i => zip(r1(i), r2(i)) }
  }

  def mean1: Process[Double, Double] = zip[Double, Int, Double](count, sum) map { case (count, sum) => sum / count }

  // Exercise 15.8
  // V1: Halting and only yielding the final result
  def exists[I](p: I => Boolean): Process[I, Boolean] = Await[I, Boolean] {
    case Some(i) => if (p(i)) Emit(true) else exists(p)
    case _ => Emit(false)
  }

  // V2: Halting at the first true and yielding all intermediate results
  def exists1[I](p: I => Boolean): Process[I, Boolean] = Await[I, Boolean] {
    case Some(i) => if (p(i)) Emit(true) else Emit(false, exists1(p))
    case _ => Halt()
  }

  // V3: Not halting and yielding all intermadiate results
  def exists2[I](p: I => Boolean): Process[I, Boolean] = Await[I, Boolean] {
    case Some(i) => if (p(i)) Emit(true, exists2(_ => true)) else Emit(false, exists2(p))
    case _ => Halt()
  }

  
  def any: Process[Boolean, Boolean] = loop(false) { (b, s) => (b || s, b || s) }

  def exists3[I](p: I => Boolean): Process[I, Boolean] = lift(p) |> any

  def echo[I]: Process[I, I] = await(i => emit(i))
  def takeThrough[I](p: I => Boolean): Process[I, I] = takeWhile(p) ++ echo

  def exist4[I](p: I => Boolean): Process[I, Boolean] = exists3(p) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

  def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = cur match {
      case Halt() => acc
      case Await(recv) =>
        val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
        go(ss, next, acc)
      case Emit(h, t) => go(ss, t, g(acc, h))
    }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }

  val f: java.io.File = ???
  val x: IO[Boolean] = processFile(f, count |> exists3(_ > 40000), false)(_ || _)

  // Exercise 15.9
  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)
  
  def convertFarhenheit: Process[String, String] =
    filter((line: String) => !line.startsWith("#")) |>
    filter(line => line.trim.nonEmpty) |>
    lift(line => toCelsius(line.toDouble).toString)

  def processToFile(i: java.io.File, o: java.io.File, p: Process[String, String]): IO[Unit] = IO {
    def go(ss: Iterator[String], w: java.io.PrintWriter, cur: Process[String, String]): Unit = cur match {
      case Halt() => ()
      case Await(recv) =>
        val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
        go(ss, w, next)
      case Emit(h, t) => {
        w.write(h)
        go(ss, w, t)
      }
    }
    val is = io.Source.fromFile(i)
    val w = new java.io.PrintWriter(o)
    try {
      go(is.getLines, w, p)
    }
    finally {
      is.close
      w.close
    }
  }

  val i: java.io.File = ???
  val o: java.io.File = ???
  val y: IO[Unit] = processToFile(i, o, convertFarhenheit)
}
