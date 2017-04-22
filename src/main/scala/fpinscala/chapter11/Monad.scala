package fpinscala.chapter11

import language.higherKinds

import fpinscala.chapter8._
import fpinscala.chapter7.Par._
import fpinscala.chapter9._
import fpinscala.chapter4._
import fpinscala.chapter6._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Right(fa) => map(fa)(Right(_))
    case Left(fb) => map(fb)(Left(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((fa, fal) => map2(fa, fal)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, fbl) => map2(f(a), fbl)(_ :: _))

  // Exercise 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))((a, fal) => flatMap(f(a))(p => if (p) map2(unit(a), fal)(_ :: _) else fal))

  /*
   * For `Par`, `filterM` filters a list, applying the functions in
   * parallel; for `Option`, it filters a list, but allows
   * the filtering function to fail and abort the filter
   * computation; for `Gen`, it produces a generator for 
   * subsets of the input list, where the function `f` picks a 
   * 'weight' for each element (in the form of a `Gen[Boolean]`)
   */
  def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ga: Gen[A])(f: A => Gen[B]): Gen[B] =
      ga flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = unit(a)
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(pa)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](pa: P[A])(f: A => P[B]): P[B] =
      p.flatMap(pa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] = as flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f
  }

  //val eitherMonad: Monad[Either] = ???

  // Exercise 11.2
  //type StateS[A] = State[_, A]
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): StateS[A] = State.unit(a)
      def flatMap[A, B](sa: StateS[A])(f: A => StateS[B]): StateS[B] =
        sa flatMap f
    }
  }
 
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
      sa flatMap f
  }

  //
}