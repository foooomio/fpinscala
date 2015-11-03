object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n <= 1) a
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatFib(n: Int) = {
    val msg = "The fibonacci value of %d is %d."
    msg.format(n, fib(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val as = Array("a", "b", "c");
    println(findFirst(as, "c"))

    val bs = Array(1, 2, 3)
    println(findFirst(bs, (x: Int) => x == 2))

//    println(formatAbs(-42))
//    println(formatFactorial(7))
//    println(formatFib(5))
  }
}
