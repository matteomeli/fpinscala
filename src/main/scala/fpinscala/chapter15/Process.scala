package fpinscala.chapter15

import language.higherKinds

import fpinscala.chapter13.Free._

trait ProcessF[F[_], O] {
  import ProcessF._

  def onHalt(f: Throwable => ProcessF[F, O]): ProcessF[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => ProcessF[F, O]): ProcessF[F, O] = this.onHalt {
    case End => p
    case err => Halt(err)
  }

  def flatMap[O2](f: O => ProcessF[F, O2]): ProcessF[F, O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
  }

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: ProcessF[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
      case Emit(h, t) => go(t, acc :+ h)
      case Halt(End) => F.unit(acc)
      case Halt(err) => F.fail(err)
      case Await(req, recv) =>
        F.flatMap(F.attempt(req))(e => go(Try(recv(e)), acc))
    }
    go(this, IndexedSeq())
  }

  def onComplete(p: => ProcessF[F, O]): ProcessF[F, O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: ProcessF[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }
}

object ProcessF {
  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => ProcessF[F, O]) extends ProcessF[F, O]
  case class Emit[F[_], O](head: O, tail: ProcessF[F, O]) extends ProcessF[F, O]
  case class Halt[F[_], O](err: Throwable) extends ProcessF[F, O]

  case object End extends Exception
  case object Kill extends Exception

  def Try[F[_], O](p: => ProcessF[F, O]): ProcessF[F, O] =
    try p
    catch { case e: Throwable => Halt(e)}

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => ProcessF[F, O]): ProcessF[F, O] =
    Await(req, recv)

  /*
  * Here is a simple tail recursive function to collect all the
  * output of a `Process[IO,O]`. Notice we are using the fact
  * that `IO` can be `run` to produce either a result or an
  * exception.
  */
  def runLog[O](src: ProcessF[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    @annotation.tailrec
    def go(cur: ProcessF[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
      cur match {
        case Emit(h,t) => go(t, acc :+ h)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(unsafePerformIO(req)(E)))
            catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
      }
    try go(src, IndexedSeq())
    finally E.shutdown
  }

  /*
     * We can write a version of collect that works for any `Monad`.
     * See the definition in the body of `Process`.
     */

  import java.io.{BufferedReader,FileReader}
  val p: ProcessF[IO, String] =
    await(IO(new BufferedReader(new FileReader("lines.txt")))) {
      case Right(b) =>
        lazy val next: ProcessF[IO,String] = await(IO(b.readLine)) {
          case Left(e) => await(IO(b.close))(_ => Halt(e))
          case Right(line) => Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

  def resource[R, O](acquire: IO[R])(use: R => ProcessF[IO, O])(release: R => ProcessF[IO, O]): ProcessF[IO, O] =
    await[IO, R, O](acquire) {
      case Left(t) => Halt(t)
      case Right(r) => use(r).onComplete(release(r))
    }
}

