object fibo extends App {
  var add = 0

  def fibonacci(n: Int): Int = {
    n match {
      case n if n == 1 || n == 0 => n
      case _ => fibonacci(n - 1) + fibonacci(n - 2)

    }
  }

  println(fibonacci(8),add)


}
