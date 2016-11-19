package fpinscala.chapter2

object GettingStarted {
  def main(args: Array[String]): Unit = {
    println("Functional Programming in Scala!")
  }

  // Exercise 2.1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)
    }
    loop(n, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  // Exercie 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
