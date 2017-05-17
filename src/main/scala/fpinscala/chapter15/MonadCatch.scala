package fpinscala.chapter15

import language.higherKinds

import fpinscala.chapter13.Monad

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]
  def fail[A](t: Throwable): F[A]
}