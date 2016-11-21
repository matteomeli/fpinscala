package fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercie 3.1 answer is 3

  // Exercise 3.2
  // In the Nil case I could have return Nil, but I choose to throw an exception because taking the tail of an empty List is isually an error and returning Nil would fail it silently
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("tail of empty list")
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => throw new NoSuchElementException("setHead of empty list")
    case Cons(_, xs) => Cons(a, xs)
  }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
    case Nil => Nil
    case Cons(_, xs)=> drop(xs, n - 1)
  }

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _ => l
  }

  // Exercise 3.6
  // TODO: Come back when reverse fn is available and write this to be tailrec accumulating the new list in reverse order and then reverse it in the end
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}
