import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

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

  def exponential_methodOne(n: Double, x:Double): Double = {
    var ans: Double = 1
    (1 to n.toInt).foreach { i =>
      ans += power(x.toInt, i) / factorial(i)
    }
    ans
  }

  def exponential_methodTwo(n: Double, x:Double): Double = {
    var ans: Double = 1
    var prev: Double = 1
    (1 to n.toInt).foreach { i =>
      prev = prev * x / i
      ans += prev
    }
    ans
  }

  def exponential_methodThree(n: Double, x:Double): Double = {
    var prev: Double = 1.0
    (n.toInt to 1 by -1).foreach { i =>
      prev = 1 + (x / i) * prev
    }
    prev
  }

  def exponential_methodFour(n:Double, x: Double): Double = {
    var exponentialOfWholeNumber: Double = 0.0
    var nTimes = 1
    var wholeNumber = x.toInt
    breakable {
      for (i <- 1 to x.toInt) {
        if (wholeNumber == 1) {
          exponentialOfWholeNumber = 1 + (1 + 1 / 2 * (1 + 1 / 3 * (1 + 1 / 4 * (1 + 1 / 5))))
          nTimes = i
          break
        }
        else
          wholeNumber = wholeNumber / 2
      }
    }
    val dble = x - wholeNumber
    exponentialOfWholeNumber += 1 + (dble + dble / 2 * (1 + dble / 3 * (1 + dble / 4 * (1 + dble / 5))))
    exponentialOfWholeNumber
  }

  def timeTakenOwnMethods(method: (Double,Double) => Double, n: Double, x:Double,  methodType: String): Long = {
    val start = System.currentTimeMillis()
//    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => method(n,x))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${method(n,x)}")
    end - start
  }

  def timeTakenLibraryMethod(n: Double, x:Double, methodType: String = "Math.Exp "): Long = {
    val start = System.currentTimeMillis()
    //    println(s"$methodType started  for n = $n")
    (0 to 1000000).foreach(_ => math.exp(x))
    val end = System.currentTimeMillis()
    println(s"$methodType ended for n = $n is ${end - start}ms and value = ${math.exp(x)}")
    end - start
  }

  def main(args: Array[String]): Unit = {
    val inputList: List[Double] = List(1.0, 5.0, 11.0, 17.0, 23.0, 29.0)
    val methodList = List("O(n) first method", "O(n) second method", "Naive method", "log(n) method")
    val methods: List[(Double,Double) => Double] = List(exponential_methodTwo,exponential_methodThree, exponential_methodOne, exponential_methodFour)
    val result: ArrayBuffer[Long] = ArrayBuffer()

    val method_wise_tuple = methods zip methodList
    val x = 2
    inputList.foreach{n =>
      method_wise_tuple.foreach{ methodTuple =>
        result.append(timeTakenOwnMethods(methodTuple._1, n,x, methodTuple._2))}
    }
    inputList.foreach{n => result.append(timeTakenLibraryMethod(n,x))}

    println(result)
    }

}
