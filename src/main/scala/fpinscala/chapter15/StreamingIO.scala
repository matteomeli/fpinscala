package fpinscala.chapter15

import fpinscala.chapter13.Free._

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
}
