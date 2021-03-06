package fpinscala.chapter9

import language.higherKinds
import language.implicitConversions
import language.postfixOps

import fpinscala.chapter8.{Gen, Prop, SGen}
import fpinscala.chapter8.Gen._
import fpinscala.chapter8.Prop._

import scala.util.matching.Regex
import java.util.regex._

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))  

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // Exercise 9.8
  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
    flatMap(pa)(a => succeed(f(a)))

  def succeed[A](a: => A): Parser[A]

  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // Exercise 9.1
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => map(p2)(b => f(a, b)))

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))

  // Exercise 9.7
  def product2[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => map(p2)(b => (a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  implicit def regex(r: Regex): Parser[String]

  def sequence[A](p: List[Parser[A]]): Parser[List[A]] =
    p.foldLeft(succeed(List[A]()))((acc, pa) => pa.map2(acc)(_ :: _))

  def skipL[A, B](p: Parser[A], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A, B](p: Parser[A], p2: => Parser[B]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def whitespace: Parser[String] = "\\s*".r

  def token[A](p: Parser[A]): Parser[A] = p << whitespace
  
  def digits: Parser[String] = "\\d+".r

  def consumeUntil(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") >> consumeUntil("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] = token(quoted) // TODO: This should actually handle escaped strings as well

  def doubleString: Parser[String] = token("[+-]?([0-9]*\\.)?[0-9]+([eE][+-]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString map (_.toDouble)

  def separated[A, B](p1: Parser[A], p2: Parser[B]): Parser[List[A]] =
    separated1(p1, p2) or succeed(List())

  def separated1[A, B](p1: Parser[A], p2: Parser[B]): Parser[List[A]] =
    map2(p1, (p2 >> p1) many)(_ :: _)

  def surround[A, B, C](start: Parser[A], stop: Parser[B])(p: => Parser[C]): Parser[C] =
    start >> p << stop

  def eof: Parser[String] = "\\z".r

  def root[A](p: Parser[A]) = p << eof

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  //def errorLocation(e: ParserError): Location
  def errorMessage(e: ParserError): String = ???

  //def furthest[A](p: Parser[A]): Parser[A]
  //def latest[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def >>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <<[B](p2: => Parser[B]): Parser[A] = self.skipR(p, p2)

    def token: Parser[A] = self.token(p)

    def separated1[B](p2: Parser[B]): Parser[List[A]] = self.separated1(p, p2)
    def separatedBy[B](p2: Parser[B]): Parser[List[A]] = self.separated(p, p2)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  object Laws {
    val c = 'a'
    run(char(c))(c.toString) == Right(c)
    val s = "string"
    run(string(s))(s) == Right(s)

    run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")

    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

    run(many(char('a')))("aaa") == Right(List('a', 'a', 'a'))
    val numA: Parser[Int] = char('a').many.map(_.size)
    run(numA)("aaa") == Right(3)
    val oneOrMoreA: Parser[Int] = char('a').many1.slice.map(_.size)

    val p: Parser[(Int, Int)] = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

    // Exercise 9.2
    def productLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      equal(product(p1, product(p2, p3)).map(unbiasR), product(product(p1, p2), p3).map(unbiasL))(in)
    //pa.map(f) ** pb.map(g) == (p1 ** p2).map { case (a, b) => (f(a), g(b))}

    val i = 10
    run(succeed(i))("whatever") == Right(i)
    
    // Exercise 9.6
    val nAs: Parser[Int] = regex("[0-9]+".r).flatMap(m => listOfN(m.toInt, char('a'))).map(_.size)
    val nAs2: Parser[Int] = for {
      digits <- "[0-9]+".r
      n = digits.toInt
      _ <- listOfN(n, char('a'))
    } yield n

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParserError =
    ParserError(List((this, msg)))

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def columnCaret = (" " * (col-1)) + "^"
}

case class ParserError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParserError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParserError =
    ParserError(latestLocation.map((_, s)).toList)

  def latestLocation: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack lastOption

  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
      context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col
}
