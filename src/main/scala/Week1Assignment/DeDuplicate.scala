package Week1Assignment

import java.util
import scala.util.Random


object DeDuplicate {
  val rand = new Random()

  def deDuplicateUsingSorted(list: List[Int]): (util.ArrayList[Int], Long) = {
    val start = System.nanoTime()
    val sortedList = list.sorted
    val deDuplicated: util.ArrayList[Int] = new util.ArrayList[Int]()
    var previous = sortedList.head
    deDuplicated.add(sortedList.head)
    sortedList.foreach { i =>
      if (i != previous) {
        deDuplicated.add(i)
        previous = i
      }
    }
    val end = System.nanoTime()
    (deDuplicated, end - start)
  }

  def deDuplicateUsingSet(list: List[Int]): (Set[Int], Long) = {
    val start = System.nanoTime()
    val deDuplicate = list.toSet
    val end = System.nanoTime()
    (deDuplicate, end - start)
  }

  def main(args: Array[String]): Unit = {
    val inputN = List(10, 100, 1000, 10000, 1000000, 10000000)
    val deDuplicateSorted = new util.ArrayList[Long]()
    val deDuplicateSet = new util.ArrayList[Long]()
    inputN.foreach { i =>
      val randomIntegers = (0 to i).map(_ => rand.nextInt(100)).toList
      deDuplicateSorted.add(deDuplicateUsingSorted(randomIntegers)._2)
      deDuplicateSet.add(deDuplicateUsingSet(randomIntegers)._2)
    }
    println(deDuplicateSorted)
    println(deDuplicateSet)
  }
}
