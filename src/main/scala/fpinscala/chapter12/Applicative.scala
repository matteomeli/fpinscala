package fpinscala.chapter12

import language.higherKinds
import scala.language.implicitConversions

import fpinscala.chapter11.Functor
import fpinscala.chapter10.Monoid
import fpinscala.chapter10.MonoidLaws.Foldable
import fpinscala.chapter6.State
import fpinscala.chapter6.State._

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

  // Exercise 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    //ofa.foldRight(unit(Map[K, V]())){ case ((k, fv), fmacc) => map2(fv, fmacc)((v, m) => m + (k -> v)) }
    ofa.foldLeft(unit(Map[K, V]())){ case (fmacc, (k, fv)) => map2(fv, fmacc)((v, macc) => macc + (k -> v)) }
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
  
  type Const[M, B] = M
  
  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero
      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }
}

// Every monad is an applicative (map2 implemented in terms of flatMap)
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]// = join(map(fa)(f)) // Implementation in terms of join

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
 
  // Exercise 12.11
  // Here all you have is `f`, which returns an `F[G[B]]`. For it to have the appropriate type to return from the argument to `G.flatMap`, you'd need to be able to "swap" the `F` and `G` types. In other words, you'd need a _distributive law_. Such an operation is not part of the `Monad` interface.
  /*def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fga)(ga => G.flatMap(ga)(a => f(a)))
    }
  }*/
}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = eea match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
      sa flatMap f
  }

  // Exercise 12.20
  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]):
  Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def join[A](fga: F[G[F[G[A]]]]): F[G[A]] = flatMap(fga)(identity)
      def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(fga)(ga => F.map(T.traverse(ga)(f) /* == F[G[G[B]]] */)(G.join) /* F[G[B]] */)
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

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(identity)

  // Exercise 12.14
  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => idMonad.unit(f(a)))(idMonad)

  import Applicative._

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]] // Get the current state, the accumulated list.
      _  <- set(a :: as) // Add the current element and set the new list as the new state.
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // Exercise 12.16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  // Exercise 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  // Exercise 12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_], A, B](fga: F[G[A]])(f: A => H[B])(implicit H: Applicative[H]): H[F[G[B]]] =
        sequence(self.map(fga)(G.map(_)(f)))
      override def sequence[H[_], A](fgha: F[G[H[A]]])(implicit H: Applicative[H]): H[F[G[A]]] =
        traverse(fgha)(identity)
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  // Exercise 12.13
  def listTraverse = new Traverse[List] {
    override def traverse[F[_], A, B](as: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      as.foldRight(F.unit(List[B]()))((a, acc) => F.map2(f(a), acc)(_ :: _))
  }

  def optionTraverse = new Traverse[Option] {
    override def traverse[F[_], A, B](oa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
      oa match {
        case None => F.unit(None)
        case Some(a) => F.map(f(a))(Some(_))
      }
  }

  def treeTraverse = new Traverse[Tree] {
    override def traverse[F[_], A, B](t: Tree[A])(f: A => F[B])(implicit F: Applicative[F]): F[Tree[B]] =
      /*t.tail.foldRight(F.map(f(t.head)) {
          b => Tree(b, List[Tree[B]]())
        }) { (ta, facc) => 
        F.map2(traverse(ta)(f)(F), facc) { (tb, acc) => 
          Tree(acc.head, tb :: acc.tail)
        }
      }*/
      F.map2(f(t.head), listTraverse.traverse(t.tail)(ta => traverse(ta)(f)))(Tree(_, _))
  }
}

case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
    OptionT(M.flatMap(value) {
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
  }
}
