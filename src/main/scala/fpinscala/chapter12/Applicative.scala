package fpinscala.chapter12

import language.higherKinds

import fpinscala.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // Primitive combinators set 2
  def unit[A](a: => A): F[A]
  // Exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a)) //  map2(fab, fa)(_(_))
  
  // Primitive combinators set 1
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    // Exercise 12.2
    //apply(apply(unit(f.curried))(fa))(fb)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    //map2(fa, unit(()))((a, _) => f(a))
    // Alternative implementation in terms of apply and unit (exercise 12.2)
    apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbl) => map2(f(a), fbl)(_ :: _))

  // Exercise 12.1
  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // Exercise 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    // Exercise 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      def map2[A, B, C](fga: (F[A], G[A]), fgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        apply(apply(unit(f.curried))(fga))(fgb)
      override def apply[A, B](fgab: (F[A => B], G[A => B]))(fga: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fgab._1)(fga._1), G.apply(fgab._2)(fga._2))
    }
  }

  // Exercise 12.9
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        apply(apply(self.unit(G.unit(f.curried)))(fga))(fgb) // == self.map2(fga, fgb)(G.map2(_, _)(f))

      override def apply[A, B](fgab: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fgab, fga)(G.map2(_, _)(_(_)))
    }
  }
}

object Applicative {
  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](sa: Stream[A], sb: Stream[B])(f: (A, B) => C): Stream[C] =
      sa zip sb map f.tupled
  }

  // Exercise 12.4
  // def sequence[A](a: List[Stream[A]]): Stream[List[A]]
  // Transforms a List of n Streams (possibly infinite in length) into a Stream of Lists containing n from correspondent index n == transpose!
}

// Every monad is an applicative (map2 implemented in terms of flatMap)
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = eea match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }
}

// Exercie 12.6
sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)
    def map2[A, B, C](vea: Validation[E, A], vab: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (vea, vab) match {
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (x@Failure(_, _), _) => x
      case (_, x@Failure(_, _)) => x
      case (Success(a), Success(b)) => Success(f(a, b))
    }
  }
}
