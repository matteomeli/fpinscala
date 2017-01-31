package fpinscala.chapter4

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMapViaMap[B](f: A => Option[B]): Option[B] =
    this map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def orElseViaMap[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filterViaFlatMap(f: A => Boolean): Option[A] =
    this flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => b map(y => f(x, y)))

  // Exercise 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    if (as.exists(_ == None)) None
    else Some(as map { case Some(a) => a }) // This assumes the input to the map will never be None

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
  }

  def sequence3[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
}
