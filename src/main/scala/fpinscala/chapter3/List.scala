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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // Exercise 3.7
  // Using foldRight it is not possible to short circuit the product algorithm when encountering 0.
  // This is because foldRight evaluates all the arguments before calling the product function on them.

  // Exercise 3.8
  // Using foldRight like this foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_)) we get back the original list. Nil becomes z, the default base and Cons is the f used to combine all the original list elements

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum2(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((acc, h) => Cons(h, acc))

  // Exercise 3.13
  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b:B) => b)((a, g) => b => g(f(b, a)))(z)

  // Exercise 3.14
  def append[A](as: List[A], a: A): List[A] = appendViaFoldRight(as, List(a))
  def append[A](l: List[A], r: List[A]): List[A] = appendViaFoldRight(l, r)
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((acc, h) => Cons(h, acc))

  // Exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)
}
