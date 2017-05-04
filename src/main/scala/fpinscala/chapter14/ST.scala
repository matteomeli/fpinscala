package fpinscala.chapter14

object Quicksort {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

object RunnableST {
  val p = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(x + 1)
      _ <- r2.write(y + 2)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  // Exercise 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](())) {
    case ((k, v), st) => st flatMap { _ => write(k, v) }
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = Array.fill(sz)(v)
  })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = xs.toArray
  })

  def noop[S]: ST[S, Unit] = ST(())

  // Exercise 14.2
  def partition[S](arr: STArray[S, Int], l: Int, r: Int, p: Int): ST[S, Int] = for {
    pv <- arr.read(p)
    _ <- arr.swap(p, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S]){ (s, i) => 
      for {
        //_ <- s // Do I need this?
        vi <- arr.read(i)
        _ <- if (vi < pv) (for {
          vj <- j.read
          _ <- arr.swap(i, vj)
          _ <- j.write(vj + 1)
        } yield ()) else noop[S]
      } yield ()
    }
    x <- j.read
    _ <- arr.swap(x, r)
  } yield x

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    p <- partition(a, l, r, l + (r - r) / 2)
    _ <- qs(a, l, p - 1)
    _ <- qs(a, p + 1, r)
  } yield () else noop[S]
  
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}