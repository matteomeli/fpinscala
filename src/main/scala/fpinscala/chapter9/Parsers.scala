package fpinscala.chapter9

import language.higherKinds

import fpinscala.chapter8.{Gen, Prop}
import fpinscala.chapter8.Gen._
import fpinscala.chapter8.Prop._


trait Parsers[ParserError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def zeroOrMore(c: Char): Parser[Int]
  def oneOrMore(c: Char): Parser[Int]

  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
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

    run(zeroOrMore('a'))("aa") == Right(2)
    run(zeroOrMore('a'))("a") == Right(1)
    run(zeroOrMore('a'))("") == Right(0)
    run(zeroOrMore('a'))("b") == Right(0)
    run(zeroOrMore('a'))("ba") == Right(0)

    run(oneOrMore('a'))("aa") == Right(2)
    run(oneOrMore('a'))("a") == Right(1)
    run(oneOrMore('a'))("") == Left("Expected one or more 'a'")
    run(oneOrMore('a'))("b") == Left("Expected one or more 'a'")
    run(oneOrMore('a'))("ba") == Left("Expected one or more 'a'")

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    val i = 10
    run(succeed(i))("whatever") == Right(i)
  }
}

case class ParserError(error: String)