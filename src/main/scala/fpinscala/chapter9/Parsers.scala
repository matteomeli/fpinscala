package fpinscala.chapter9

import language.higherKinds

import fpinscala.chapter8.{Gen, Prop}
import fpinscala.chapter8.Gen._
import fpinscala.chapter8.Prop._

import scala.util.matching.Regex

trait Parsers[ParserError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))  

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

  def succeed[A](a: A): Parser[A] =
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

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
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
      val n = digits.toInt
      _ <- listOfN(n, char('a'))
    } yield n
  }
}


case class ParserError(msg: String)