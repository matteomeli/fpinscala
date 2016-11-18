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
}
