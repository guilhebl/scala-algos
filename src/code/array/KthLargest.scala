package code.array

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.geeksforgeeks.org/k-largestor-smallest-elements-in-an-array/
  *
  * For example, if given array is
  * [1, 23, 12, 9, 30, 2, 50]
  * and you are asked for the largest 3 elements i.e., k = 3 then your program should print 50, 30 and 23.
  */
object KthLargest {
  def main(args: Array[String]): Unit = {
    val arr = Array(1, 23, 12, 9, 30, 2, 50)

    println(printLargestK(arr, 3))
  }

  def printLargestK(ints: Array[Int], k: Int) : Option[Seq[Int]] = {

    if (ints.length < k) {
      return None
    }

    val pq = mutable.PriorityQueue.empty[Int] ++ ints
    var results = ListBuffer[Int]()

    for (_ <- 1 to k) {
      results += pq.dequeue()
    }

    Some(results)
  }
}
