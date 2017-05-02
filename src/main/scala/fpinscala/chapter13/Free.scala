package fpinscala.chapter13

import language.higherKinds

sealed trait Free[F[_], A] {
  // Exercise 13.1
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  // Exercise 13.1
  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a flatMap f
  }

  // Exercise 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(x) => x
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(y) => runTrampoline { f(y) }
      case Suspend(t) => runTrampoline { f(t()) }
      case FlatMap(y, g) => runTrampoline { y.flatMap(a => g(a).flatMap(f)) } // == This should be equal to a.flatMap(g).flatMap(f) for the monadic associativity law 
    }
  }

  // Exercise 13.3
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, g), f) => step(x.flatMap(a => g(a).flatMap(f)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(x) => F.unit(x)
    case Suspend(s) => F.flatMap(s)(a => F.unit(a))
    case FlatMap(Suspend(t), f) => F.flatMap(t)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  import fpinscala.chapter7.Par
  sealed trait Console[A] {
    def toPar: Par.Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] =
      try Some(io.StdIn.readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(s: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(s))
    def toThunk = () => println(s)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(s: String): ConsoleIO[Unit] = Suspend(PrintLine(s))
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar = new (Console ~> Par.Par) { def apply[A](a: Console[A]) = a.toPar }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
    step(free) match {
      case Return(x) => G.unit(x)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)(function0Monad)

  def runConsoleToPar[A](a: Free[Console, A]): Par.Par[A] =
    runFree[Console, Par.Par, A](a)(consoleToPar)(parMonad)

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par.Par] {
    def unit[A](a: => A) = Par.lazyUnit(a)
    def flatMap[A, B](pa: Par.Par[A])(f: A => Par.Par[B]): Par.Par[B] =
      Par.fork { pa.flatMap(f) }
  }

  // Exercise 13.4
  implicit val freeFunction0Monad = freeMonad[Function0]
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](f: F[A]): FreeG[A] = Suspend { fg(f) }
    }
    runFree(f)(t)(freeMonad[G])
  }
  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(consoleToFunction0))

  // Exercise 13.5
  import fpinscala.chapter7.Nonblocking._
  import fpinscala.chapter7.Nonblocking.Par
  import java.nio._
  import java.nio.channels._

  type IO[A] = Free[Par, A]

  // Provides the syntax `Async { k => ... }` for asyncronous IO blocks.
  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend { async(cb) }

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend { delay(a) }

  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = {
    async {
      (cb: Either[Throwable, Array[Byte]] => Unit) => {
        val buffer = ByteBuffer.allocate(numBytes)
        file.read(buffer, fromPosition, (), new CompletionHandler[Integer, Unit]{
          def completed(bytesRead: Integer, u: Unit): Unit = {
            val array = new Array[Byte](bytesRead)
            buffer.slice.get(array, 0, bytesRead)
            cb(Right(array))
          }

          def failed(e: Throwable, u: Unit): Unit = {
            cb(Left(e))
          }
        })
      }
    }
  }
}
