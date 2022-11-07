import scala.collection.mutable.ArrayBuffer

object Exponential {

  def power(base: Double, pow: Int): Double = {
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

  def exponential_methodOne(x: Double): Double = {
    var ans: Double = 1
    (1 to 30).foreach { i =>
      ans += power(x, i) / factorial(i)
    }
    ans
  }

  def exponential_methodTwo(x: Double): Double = {
    var ans: Double = 1
    var prev: Double = 1
    (1 to 200).foreach { i =>
      prev = prev * x / i
      ans += prev
    }
    ans
  }

  def exponential_methodThree(x: Double): Double = {
    var prev: Double = 1.0
    (700 to 1 by -1).foreach { i =>
      prev = 1 + (x / i) * prev
    }
    prev
  }

  def timeTaken(method: Double => Double, n: Double, methodType: String): Long = {
    val start = System.currentTimeMillis()
    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => method(n))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${method(n)}")
    end - start
  }

  def main(args: Array[String]): Unit = {
    val inputList: List[Double] = List(5.0, 11.0, 17.0, 23.0, 29.0)
    val methodList = List("Library method", "O(n) first method", "O(n) second method", "Naive method")
    val methods: List[Double => Double] = List(math.exp, exponential_methodThree, exponential_methodTwo, exponential_methodOne)
    val result: ArrayBuffer[Long] = ArrayBuffer()

    val method_wise_tuple = methods zip methodList
    inputList.foreach{n =>
      method_wise_tuple.foreach{ methodTuple => result.append(timeTaken(methodTuple._1, n, methodTuple._2))}
    }
    println(result)
    }

}
