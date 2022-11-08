import scala.collection.mutable.ArrayBuffer

object Exponential {

  def power(base: Int, pow: Int): Long = {
    var ans: Long = 1
    (1 to pow).foreach { _ =>
      ans *= base
    }
    ans
  }

  def factorial(factOf: Long): Long = {
    var ans: Long = 1
    (factOf until 1 by -1).foreach { i =>
      ans *= i
    }
    ans
  }

  def exponential_methodOne(x: Double): Double = {
    var ans: Double = 1
    (1 to x.toInt).foreach { i =>
      ans += power(2, i) / factorial(i)
    }
    ans
  }

  def exponential_methodTwo(x: Double): Double = {
    var ans: Double = 1
    var prev: Double = 1
    (1 to x.toInt).foreach { i =>
      prev = prev * 2 / i
      ans += prev
    }
    ans
  }

  def exponential_methodThree(x: Double): Double = {
    var prev: Double = 1.0
    (x.toInt to 1 by -1).foreach { i =>
      prev = 1 + (2 / i) * prev
    }
    prev
  }

  def timeTaken(method: Double => Double, n: Double, methodType: String): Long = {
    val start = System.currentTimeMillis()
//    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => method(2))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${method(2)}")
    end - start
  }

  def main(args: Array[String]): Unit = {
    val inputList: List[Double] = List(1.0, 5.0, 11.0, 17.0, 23.0, 29.0)
    val methodList = List("Library method", "O(n) first method", "O(n) second method", "Naive method")
    val methods: List[Double => Double] = List(math.exp, exponential_methodTwo,exponential_methodThree, exponential_methodOne)
    val result: ArrayBuffer[Long] = ArrayBuffer()

    val method_wise_tuple = methods zip methodList
    inputList.foreach{n =>
      method_wise_tuple.foreach{ methodTuple => result.append(timeTaken(methodTuple._1, n, methodTuple._2))}
    }
    println(result)
    }

}
