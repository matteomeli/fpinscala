package fpinscala.chapter15

import language.higherKinds
import language.postfixOps

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

  def drain[O2]: ProcessF[F, O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(_, t) => t.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))
  }

  def map[O2](f: O => O2): ProcessF[F, O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(o, t) => Emit(f(o), t.map(f))
    case Await(req, recv) => Await(req, recv andThen (_.map(f)))
  }

  def filter(p: O => Boolean): ProcessF[F, O] = this match {
    case Halt(e) => Halt(e)
    case Emit(o, t) if p(o) => Emit(o, t.filter(p))
    case Emit(_, t) => t.filter(p)
    case Await(req, recv) => Await(req, recv andThen (_.filter(p)))
  }

  def repeat: ProcessF[F, O] = this ++ this.repeat

  def |>[O2](p: Process1[O, O2]): ProcessF[F, O2] = p match {
    case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
    case Emit(o2, t) => Emit(o2, this |> t)
    case Await(req, recv) => this match {
      case Halt(e) => Halt(e) |> recv(Left(e))
      case Emit(o, t) => t |> Try(recv(Right(o)))
      case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p))
    }
  }

  def pipe[O2](p: Process1[O, O2]): ProcessF[F, O2] = this |> p

  def filter2(p: O => Boolean): ProcessF[F, O] =
    this |> ProcessF.filter(p)

  @annotation.tailrec
  final def kill[O2]: ProcessF[F, O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(_, t) => t.kill
    case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
      case Kill => Halt(End)
      case e => Halt(e)
    }
  }

  def tee[O2,O3](p2: ProcessF[F,O2])(t: Tee[O,O2,O3]): ProcessF[F,O3] = {
    t match {
      case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h,t) => Emit(h, (this tee p2)(t))
      case Await(side, recv) => side.get match {
        case Left(isO) => this match {
          case Halt(e) => p2.kill onComplete Halt(e)
          case Emit(o,ot) => (ot tee p2)(Try(recv(Right(o))))
          case Await(reqL, recvL) =>
            await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
        }
        case Right(isO2) => p2 match {
          case Halt(e) => this.kill onComplete Halt(e)
          case Emit(o2,ot) => (this tee ot)(Try(recv(Right(o2))))
          case Await(reqR, recvR) =>
            await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
        }
      }
    }
  }

  def zipWith[O2,O3](p2: ProcessF[F,O2])(f: (O,O2) => O3): ProcessF[F,O3] =
    (this tee p2)(ProcessF.zipWith(f))

  def zip[O2](p2: ProcessF[F,O2]): ProcessF[F,(O,O2)] =
    zipWith(p2)((_, _))

  def to[O2](sink: Sink[F, O]): ProcessF[F, Unit] =
    join { (this zipWith sink)((o, f) => f(o)) }
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

  def emit[F[_], O](h: O, t: ProcessF[F, O] = Halt[F, O](End)): ProcessF[F, O] = Emit(h, t)

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
  
  // Exercise 15.11
  def eval[F[_], A](a: F[A]): ProcessF[F, A] = await[F, A, A](a) {
    case Left(t) => Halt(t)
    case Right(a) => Emit(a, Halt(End))
  }

  def eval_[F[_], A, B](a: F[A]): ProcessF[F, B] = eval(a).drain[B]

  /*
  * Create a `Process[IO,O]` from the lines of a file, using
  * the `resource` combinator above to ensure the file is closed
  * when processing the stream of lines is finished.
  */
  def lines(filename: String): ProcessF[IO, String] =
    resource
      { IO(io.Source.fromFile(filename)) }
      { src =>
          lazy val iter = src.getLines // a stateful iterator
          def step = if (iter.hasNext) Some(iter.next) else None
          lazy val lines: ProcessF[IO, String] = eval(IO(step)).flatMap {
            case None => Halt(End)
            case Some(line) => Emit(line, lines)
          }
          lines
      }
      { src => eval_ { IO(src.close) } }

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }
  def Get[I] = Is[I]().Get

  type Process1[I, O] = ProcessF[Is[I]#f, O]

  def await1[I, O](recv: I => Process1[I, O], fallback: => Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i) => recv(i)
    })

  def halt1[I, O]: Process1[I, O] = Halt(End)

  def emit1[I, O](h: O, t: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    emit(h, t)

  def lift1[I, O](f: I => O): Process1[I, O] =
    await1[I, O](i => emit1(f(i))) repeat

  def filter[I](p: I => Boolean): Process1[I, I] =
    await1[I, I](i => if (p(i)) emit1(i) else halt1) repeat

  def id[I]: Process1[I,I] =
    await1((i: I) => emit(i, id))

  def intersperse[I](sep: I): Process1[I, I] =
    await1(i => emit1(i) ++ id.flatMap(i => emit(sep) ++ emit(i)))

  case class T[I,I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
    val L = new f[I] { def get = Left(identity) }
    val R = new f[I2] { def get = Right(identity) }
  }
  def L[I,I2] = T[I,I2]().L
  def R[I,I2] = T[I,I2]().R

  type Tee[I,I2,O] = ProcessF[T[I,I2]#f, O]

  def haltT[I,I2,O]: Tee[I,I2,O] =
    Halt[T[I,I2]#f,O](End)

  def awaitL[I,I2,O](recv: I => Tee[I,I2,O], fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    await[T[I,I2]#f,I,O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def awaitR[I,I2,O](recv: I2 => Tee[I,I2,O], fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    await[T[I,I2]#f,I2,O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    emit(h, tl)

  def zipWith[I,I2,O](f: (I,I2) => O): Tee[I,I2,O] =
    awaitL[I,I2,O](i  =>
    awaitR        (i2 => emitT(f(i,i2)))) repeat

  def zip[I,I2]: Tee[I,I2,(I,I2)] = zipWith((_,_))

  type Sink[F[_], O] = ProcessF[F, O => ProcessF[F, Unit]]

  import java.io.FileWriter
  def fileW(file: String, append: Boolean = false): Sink[IO, String] =
    resource[FileWriter, String => ProcessF[IO, Unit]]
      { IO { new FileWriter(file, append) }}
      { w => constant { (s: String) => eval[IO, Unit](IO(w.write(s))) }}
      { w => eval_(IO(w.close)) }

  def constant[A](a: A): ProcessF[IO, A] =
    eval(IO(a)).flatMap { a => Emit(a, constant(a)) }

  // Exercise 15.12
  def join[F[_], O](p: ProcessF[F, ProcessF[F, O]]): ProcessF[F, O] =
    p.flatMap(identity)

  def fahrenheitToCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)
  val converter: ProcessF[IO, Unit] =
    lines("fahrenheit.txt").
    filter(line => !line.startsWith("#") && !line.trim.isEmpty).
    map(line => fahrenheitToCelsius(line.toDouble).toString).
    pipe(intersperse("\n")).
    to(fileW("celsius.txt")).
    drain
}

