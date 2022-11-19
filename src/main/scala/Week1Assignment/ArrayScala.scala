package Week1Assignment
import java.util
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object ArrayScala {
  def test(n: Int): Unit = {
    // our array list; remember this is not a list, but a dynamic array that suffers the perils of inserts and deletes
    val list = new util.ArrayList[Int]()
    // let's use a random number generator
    val rand = new Random()
    // repeat the exercise for n inserts
    for (i <- 0 to n) {
      // get a random integer
      var r = rand.nextInt
      val start = System.nanoTime
      // insert it to the array.
      list.add(r)
      // measure the time taken in micro seconds to perform the insert
      val diff = (System.nanoTime - start) / 1000
      // if the difference is >10us, let's observe it.
      // the time taken is for rebuilding the array to ensure continuity.
      if (diff > 10) print(s" ${list.size} \t $diff \n")
    }
  }

  def averageTime(n: Int): Double = {
    val TimeTaken: ArrayBuffer[Long] = new ArrayBuffer[Long]()
    val list = new util.ArrayList[Int]()
    // let's use a random number generator
    val rand = new Random()
    // repeat the exercise for n inserts
    for (i <- 0 to n) {
      // get a random integer
      var r = rand.nextInt
      val start = System.nanoTime
      // insert it to the array.
      list.add(r)
      // measure the time taken in micro seconds to perform the insert
      val diff = (System.nanoTime - start) / 1000
      TimeTaken.append(diff)
    }
    TimeTaken.sum.toDouble / n
  }

  def main(args: Array[String]): Unit = {
    val nList = List(10, 100,1000,10000, 100000, 1000000, 10000000, 100000000)
    val averageTimeTaken:ArrayBuffer[Double] = new ArrayBuffer[Double]()
    nList.foreach{n => averageTimeTaken.append(averageTime(n))}
    println(averageTimeTaken)
//    test(100000)
  }
}
