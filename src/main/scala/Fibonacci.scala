object Fibonacci {
  def main(args: Array[String]): Unit = {
    println(fibonacci(1))
    println(fibonacci(2))
    println(fibonacci(3))
    println(fibonacci(4))
    println(fibonacci(5))
    println(fibonacci(6))
    println(fibonacci(7))
    println(fibonacci(8))
    println(fibonacci(9))
    println(fibonacci(10))
  }

  def fibonacci (n : Int): Int  = {
    @annotation.tailrec
    def fib(n: Int, a: Int, b: Int): Int = {
      if (n == 3) a
      else fib(n - 1, a + b, a)
    }
    if (n == 1) 0
    else if (n == 2) 1
    else fib (n, 1, 1)
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }


}
