package fpinscala.chapter2

object GettingStarted {
  def main(args: Array[String]): Unit = {
    println("Functional Programming in Scala!")
  }

  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }
}
