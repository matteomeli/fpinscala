package fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercie 3.1 answer is 3

  // Exercise 3.2
  // In the Nil case I could have return Nil, but I choose to throw an exception because taking the tail of an empty List is isually an error and returning Nil would fail it silently
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("tail of empty list")
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => throw new NoSuchElementException("setHead of empty list")
    case Cons(_, xs) => Cons(a, xs)
  }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
    case Nil => Nil
    case Cons(_, xs)=> drop(xs, n - 1)
  }

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /*
   * Besides being inefficient, the above natural recursive solution for init() will use a stack frame for each element
   * of the list, which can lead to stack overflows for large lists (can you see why?).  lists, it's common to use
   * a temporary, mutable buffer internal to the function ( lazy lists or streams, which we discuss in chapter 5,
   * we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not
   * observable and RT (referential transparency) is preserved.
   *
   * Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
   * doesn't require even local mutation. We'll write a reverse function later in this chapter.
   */
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // Exercise 3.7
  // Using foldRight it is not possible to short circuit the product algorithm when encountering 0.
  // This is because foldRight evaluates all the arguments before calling the product function on them.

  // Exercise 3.8
  // Using foldRight like this foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_)) we get back the original list. Nil becomes z, the default base and Cons is the f used to combine all the original list elements

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum2(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((acc, h) => Cons(h, acc))

  // Exercise 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRightViaFoldLeftAlternative[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b:B) => b)((a, g) => b => g(f(b, a)))(z)

  // Exercise 3.14
  def append[A](as: List[A], a: A): List[A] = appendViaFoldRight(as, List(a))
  def append[A](l: List[A], r: List[A]): List[A] = appendViaFoldRight(l, r)
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((acc, h) => Cons(h, acc))

  // Exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, tail) => Cons((h + 1), addOne(tail))
  }

  def addOneViaFoldRight(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  // Exercise 3.17
  def doublesToStrings(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t))

  def mapStackSafe[A, B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, Nil:List[B])((h, t) => Cons(f(h), t))

  // Implementation using local mutation (variation 2).
  // Note the mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  def mapVithLocalMutation[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filterStackSafe[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filterWithLocalMutation[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercixe 3.22
  def addPairs(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairs(xs, ys))
  }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // Exercise 3.24
  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if (x == y) => startsWith(xs, ys)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
