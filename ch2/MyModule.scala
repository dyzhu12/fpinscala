object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n


  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  
  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)

  }

  def fibonacci(n: Int): Int = {

    def go(n: Int): Int =
      if (n <= 1) n
      else go(n-1)+go(n-2)

    go(n)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = { 
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (as.length == 0 || as.length == 1) true
      else if (n == as.length-1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B=> C, g: A => B): A => C = {

    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
