package fpinscala.chapter6

// Exercise 6.10
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f( a, b), s3)
  })

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    g(a).run(s2)
  })

  def mapViaFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2ViaFlatmap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight[State[S, List[A]]](unit(List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequenceRec[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def go(s: S, sas: List[State[S, A]], acc: List[A]): (List[A], S) = sas match {
      case Nil => (acc.reverse, s)
      case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
    }
    State(s => go(s, l, List()))
  }

  def sequenceViaLocalMutation[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    val buf = scala.collection.mutable.ListBuffer[A]()
    @annotation.tailrec
    def go(s: S, sas: List[State[S, A]]): (List[A], S) = sas match {
      case Nil => (buf.toList.reverse, s)
      case h :: t => h.run(s) match { case (a, s2) => buf += a; go(s2, t) }
    }
    State(s => go(s, l))
  }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
