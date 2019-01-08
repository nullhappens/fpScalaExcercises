object Chapter2 extends App {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  //non tail recursive fibonacci
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }
  //tail recursive fibonacci
  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, acc: Int, x: Int): Int =
      if (i <= 0) acc
      else go(i - 1, x, acc + x)
    go(n, 0, 1)
  }

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (as.length == 0 || n == as.length - 1)
        true
      else {
        if (ordered(as(n), as(n+1)))
          loop(n + 1)
        else
          false
      }
    }
    loop(0)
  }

  println(fib(2))
  println(fib(3))
  println(fib2(2))
  println(fib2(3))
  println(formatResult("absolute value", -42, abs))
  println(formatResult("factorial", 7, factorial))

  val a1 = Array(1, 2, 3, 4, 5)
  val f1 = (x: Int, y: Int) => x <= y
  val a2 = Array(2, 1, 4, 5)
  val a3 = Array()

  assert(isSorted(a1, f1))
  assert(!isSorted(a2, f1))
  assert(isSorted(a3, f1))
}
