package fpinscala.chapter9

import BasicParserTypes._
import scala.util.matching.Regex

object BasicParserTypes {
  trait Result[+A] {
    def mapError(f: ParserError => ParserError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def commit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, m + n)
      case _ => this
    }
  }
  case class Success[+A](get: A, charConsumed: Int) extends Result[A]
  case class Failure(get: ParserError, isCommitted: Boolean) extends Result[Nothing]

  def findFirstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    if (s1.length - offset < s2.length) s1.length - offset
    else {
      val firstNonMatchIndex = s1.substring(offset).zip(s2).takeWhile { case (c1, c2) => c1 == c2 }.size
      if (s2.size == firstNonMatchIndex) -1 else firstNonMatchIndex
    }
  }
}

case class BasicParser[+A](run: Location => Result[A])

object BasicParsers extends Parsers[BasicParser] {
  def string(s: String): BasicParser[String] = BasicParser {
    (loc: Location) =>
      val i = findFirstNonMatchingIndex(loc.input, s, loc.offset)
      if (i == -1) Success(s, s.length)
      else Failure(loc.copy(offset = loc.offset + i).toError("Expected: $s"), i != 0)
  }

  def run[A](p: BasicParser[A])(input: String): Either[ParserError, A] = p.run(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(e, _) => Left(e)
  }

  def regex(r: Regex): BasicParser[String] = BasicParser {
    (loc: Location) =>
      r.findPrefixOf(loc.input.substring(loc.offset)) match {
        case None => Failure(loc.toError("regex " + r), false)
        case Some(s) => Success(s, s.length)
      }
  }

  def succeed[A](a: A): BasicParser[A] = BasicParser {
    (loc: Location) => Success(a, 0)
  }

  def slice[A](p: BasicParser[A]): BasicParser[String] = BasicParser {
    (loc: Location) =>
      p.run(loc) match {
        case f@Failure(_, _) => f
        case Success(_, n) => Success(loc.input.substring(loc.offset, loc.offset + n), n)
      }
  }

  def scope[A](msg: String)(p: BasicParser[A]): BasicParser[A] = BasicParser {
    (loc: Location) => p.run(loc) mapError(_.push(loc, msg))
  }

  def label[A](msg: String)(p: BasicParser[A]): BasicParser[A] = BasicParser {
    (loc: Location) => p.run(loc) mapError(_.label(msg))
  }

  def attempt[A](p: BasicParser[A]): BasicParser[A] = BasicParser {
    (loc: Location) => p.run(loc).uncommit
  }

  def or[A](p1: BasicParser[A], p2: => BasicParser[A]): BasicParser[A] = BasicParser {
    (loc: Location) => p1.run(loc) match {
      case Failure(_, false) => p2.run(loc)
      case r => r
    }
  }

  def flatMap[A, B](a: BasicParser[A])(g: A => BasicParser[B]): BasicParser[B] = BasicParser {
    (loc: Location) => a.run(loc) match {
        case f@Failure(_, _) => f
        case Success(a, n) => g(a).run(loc.copy(offset = loc.offset + n)).commit(n != 0).advanceSuccess(n)
      }
  }
}