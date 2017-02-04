package fpinscala.chapter5

import Stream._

sealed trait Stream[+A] {
  // Exercise 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toList1: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def toListViaLocalMutation: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case Empty => buf.toList
    }
    go(this)
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => {
      val hh = h()
      if (p(hh)) cons(hh, t() takeWhile p) else empty
    }
  }

  // Not stack-safe, will overflow for large Streams
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || (t() exists p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    this.foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5.6
  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }

  def headOptionViaFoldRight: Option[A] =
    this.foldRight[Option[A]](None)((h, _) => Some(h))

  // Exercis 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h) append t)

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)){
      case (1, Cons(h, t)) => Some((h(), (0, empty)))
      case (n, Cons(h, t)) if n > 1 => Some((h(), (n-1, t())))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] = zipWith(s)((_, _))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s)((_, _))

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (empty, t()))
      case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), empty))
      case _ => None
    }

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll { case (x, y) => x == y }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val lacc = acc
      val b = f(a, lacc._1)
      (b, cons(b, lacc._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def from1(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => n, () => from(n + 1))
    tail
  }

  // Exercise 5.10
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // Exercise 5.12
  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)){ case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}
