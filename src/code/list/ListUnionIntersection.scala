package code.list

import scala.collection.mutable

/**
  * Input:
  * List1: 10->15->4->20
  * lsit2:  8->4->2->10
  *
  * Output:
  * Intersection List: 4->10
  * Union List: 2->8->20->4->15->10
  */
object ListUnionIntersection {

  def main(args: Array[String]): Unit = {
    val l1 = List(10, 15, 4, 20)
    val l2 = List(8, 4, 2, 10)

    // calculate occurences of elements in first list then compare with second if present insert in queue
    val occ = countOccurrences(l1)
    var intersect = mutable.Queue[Int]()
    l2.foreach(n => if (occ.contains(n)) intersect += n)

    // simply add both collections
    val union = l1 ++ l2

    println("UNION = " + union)
    println("INTERSECTION = " + intersect.toList)
  }

  def countOccurrences(list: List[Int]) : mutable.Map[Int, Int] = {
    val occ = mutable.Map[Int, Int]()
    list.foreach(n => occ.put(n, occ.getOrElse(n, 0) + 1))
    occ
  }
}
