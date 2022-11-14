import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object Exponential {

  def power(base: Double, pow: Int): Double = {
    var ans: Double = 1
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

  def exponential_methodOne(n: Double, x: Double): Double = {
    var ans: Double = 1
    (1 to n.toInt).foreach { i =>
      ans += power(x, i) / factorial(i)
    }
    ans
  }

  def exponential_methodTwo(n: Double, x: Double): Double = {
    var ans: Double = 1
    var prev: Double = 1
    (1 to n.toInt).foreach { i =>
      prev = prev * x / i
      ans += prev
    }
    ans
  }

  def exponential_methodThree(n: Double, x: Double): Double = {
    var prev: Double = 1.0
    (n.toInt to 1 by -1).foreach { i =>
      prev = 1 + (x / i) * prev
    }
    prev
  }

  def exponentialWholeNumber(n: Int): Double = {
    val binaryString = n.toBinaryString
    val exponentialMap = scala.collection.mutable.Map(0 -> 1.0, 1 -> math.E)
    var exponentOfWholeNumber = 1.0
    var ePower = 1
    n match {
      case 0 => exponentialMap(0)
      case 1 => exponentialMap(1)
      case _ => {
        (binaryString.length - 1 to 0 by -1).foreach { i =>
          if (binaryString(i) == '0') {
            ePower *= 2
            exponentialMap(ePower) = exponentialMap(ePower / 2) * exponentialMap(ePower / 2)
          }
          else if (binaryString(i) == '1') {
            ePower *= 2
            exponentialMap(ePower) = exponentialMap(ePower / 2) * exponentialMap(ePower / 2)
            exponentOfWholeNumber *= exponentialMap(ePower / 2)
          }
        }
        exponentOfWholeNumber
      }
    }
  }

  def exponentialDecimal(n: Double): Double = {
    1 + n * (1 + n / 2 * (1 + n / 3 * (1 + n / 4 * (1 + n / 5))))
  }

  def exponentialMethodFour(n: Double, x:Double): Double = {
    val wholeNumber = x.toInt
    val decimalNumber = x - wholeNumber
    val result = exponentialWholeNumber(wholeNumber) * exponentialDecimal(decimalNumber)
    result
  }

  def timeTakenMethodFour(n: Double, x: Double, methodType: String): Long = {
    val start = System.currentTimeMillis()
    val wholeNumber = x.toInt
    val decimalNumber = x - wholeNumber
    val result = exponentialWholeNumber(wholeNumber) * exponentialDecimal(decimalNumber)
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${result}")
    end - start
  }

  def timeTakenOwnMethods(method: (Double, Double) => Double, n: Double, x: Double, methodType: String): Long = {
    val start = System.currentTimeMillis()
    //    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => method(n, x))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${method(n, x)}")
    end - start
  }

  def timeTakenLibraryMethod(n: Double, x: Double, methodType: String = "Math.Exp "): Long = {
    val start = System.currentTimeMillis()
    //    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => math.exp(x))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${math.exp(x)}")
    end - start
  }

  def main(args: Array[String]): Unit = {

    val inputList: List[Double] = List(5.0, 11.0, 17.0, 23.0, 29.0)
    val methodList = List("O(n) first method", "O(n) second method", "Naive method", "nlog(n) method")
    val methods: List[(Double, Double) => Double] = List(exponential_methodTwo, exponential_methodThree, exponential_methodOne, exponentialMethodFour)
    val result: ArrayBuffer[Long] = ArrayBuffer()

    val method_wise_tuple = methods zip methodList
    val x = 5.3
    inputList.foreach { n =>
      method_wise_tuple.foreach { methodTuple =>
        result.append(timeTakenOwnMethods(methodTuple._1, n, x, methodTuple._2))
      }
    }
    inputList.foreach { n => result.append(timeTakenLibraryMethod(n, x, "math.exp")) }
    println(result)
  }

}
