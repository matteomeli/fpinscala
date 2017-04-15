package fpinscala.chapter7

import java.util.concurrent._

object Par {
  // Exercise 7.2
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIFRunning: Boolean) = false
  }

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  // Exercise 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  // Exercise 7.3
  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit) =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // Exercise 7.5
  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List(): List[A]))((a, b) => map2(a, b)(_ :: _))

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = as.map(asyncF(f))
    sequence(fbs)
  }

  def parMap[A, B](v: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(v.map(asyncF(f)))

  // Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[(A, Boolean)]] = as.map(asyncF(a => (a, f(a))))
    fbs.foldRight(unit(List() : List[A]))((x, y) =>
      map2(x, y)((r, acc) => {
        if (r._2) r._1 :: acc
        else acc
      }))
  }

  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[Option[A]]] = as.map(asyncF(a => if (f(a)) Some(a) else None))
    map(sequence(fbs))(_.flatten) // flatten called on a List[Option[A]] will remove None values
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // TODO: Define traverse?

  def equal[A](e: ExecutorService)(p: Par[A], q: Par[A]): Boolean =
    p(e).get == q(e).get

  def equal[A](p: Par[A], q: Par[A]): Par[Boolean] = Par.map2(p, q)(_ == _)

  def choice[A](cond: Par[Boolean])(tf: Par[A], ff: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) tf(es)
      else ff(es)

  // Exercise 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get))

  def choiceViaChoiceN[A](cond: Par[Boolean])(tf: Par[A], ff: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(tf, ff))

  // Exercise 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  // Define as `chooser` in the exercises but this is actually our good old friend flatMap
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val s = run(es)(pa).get
      run(es)(f(s))
    }

  // Exercise 7.13
  def choiceViaFlatMap[A](cond: Par[Boolean])(tf: Par[A], ff: Par[A]): Par[A] =
    flatMap(cond)(a => if (a) tf else ff)

  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(i => choices(i))

  def choiceViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(k => choices(k))

  // Exercise 7.14
  // This would be the equivalent of a flatten kind function for Lists
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](fa: Par[Par[A]]): Par[A] =
    flatMap(fa)(identity)

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}
