package fpinscala.chapter4

// Exercise 4.8: Use a new data structure to accumulate errors
sealed trait Partial[+A, +B] {
  def map[C](f: B => C): Partial[A, C] = this match {
    case Errors(e) => Errors(e)
    case Success(b) => Success(f(b))
  }

  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(e) => Errors(e)
    case Success(b) => f(b)
  }

  def orElse[AA >: A, C >: B](c: => Partial[AA, C]): Partial[AA, C] = (this, c) match {
    case (Success(b), _) => Success(b)
    case (Errors(b), Errors(cc)) => Errors(b ++ cc)
    case _ => c
  }

  def map2[AA >: A, C, D](c: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = (this, c) match {
    case (Success(b), Success(c)) => Success(f(b, c))
    case (Errors(a1), Errors(a2)) => Errors(a1 ++ a2)
    case (Errors(a1), _) => Errors(a1)
    case (_, Errors(a2)) => Errors(a2)
  }
}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
case class Success[+B](get: B) extends Partial[Nothing, B]

object Partial {
  def traverse[A, B, C](bs: List[B])(f: B => Partial[A, C]): Partial[A, List[C]] =
    bs.foldRight[Partial[A, List[C]]](Success(Nil))((x, y) => f(x).map2(y)(_ :: _))

  def sequence[A, B](ps: List[Partial[A, B]]): Partial[A, List[B]] = traverse(ps)(x => x)

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val age: Int)

  def mkName(name: String): Partial[String, Name] =
    if (name == "" || name == null) Errors(List("Name is empty."))
    else Success(new Name(name))

  def mkAge(age: Int): Partial[String, Age] =
    if (age < 0) Errors(List("Age is out of range."))
    else Success(new Age(age))

  def mkPerson(name: String, age: Int): Partial[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
