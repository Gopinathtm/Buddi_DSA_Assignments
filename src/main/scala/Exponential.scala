object Exponential {

  def power(base: Long, pow: Int): Double = {
    var ans: Double = 1.0
    (1 to pow).foreach { _ =>
      ans *= base
    }
    ans
  }

  def factorial(factOf: Long): Double = {
    var ans: Double = 1.0
    (factOf until 1 by -1).foreach { i =>
      ans *= i
    }
    ans
  }

  def exponential_methodOne(x: Long): Double = {
    var ans: Double = 1
    (1 to 100).foreach { i =>
      ans += power(x, i) / factorial(i)
    }
    ans
  }

  def exponential_methodTwo(x: Int): Double = {
    var ans: Double = 1
    var prev: Double = 1
    (1 to 700).foreach { i =>
      prev = prev * x / i
      ans += prev
    }
    ans
  }

  def exponential_methodThree(x: Double): Double = {
    var prev: Double = 1
    (700 to 1 by -1).foreach { i =>
      prev = 1 + (x / i) * prev
    }
    prev
  }

  def main(args: Array[String]): Unit = {
    val n = 29
    println(s"Naive method ${exponential_methodOne(n)}")
    println(s"Standard library ${math.exp(n)}")
    println(s"Method Two ${exponential_methodTwo(n)}")
    println(s"Method Three ${exponential_methodThree(n)}")


  }

}
