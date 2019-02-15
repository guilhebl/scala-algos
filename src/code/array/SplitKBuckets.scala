package code.array

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, PriorityQueue}

case class Country(players: Int, name: String)

/**
  https://careercup.com/question?id=5705185389707264
 */
object SplitKBuckets {

  def main(args: Array[String]): Unit = {

    println(maxGroupsK(
      List(
      Country(4, "USA"),
      Country(6, "CHI"),
      Country(6, "IND"),
      Country(3, "BRA"),
      Country(2, "AUS"),
      Country(1, "CAN")),
      3
    ))

  }

  def sortByPlayers(c: Country) = c.players

  def maxGroupsK(countries: List[Country], k: Int): Int = {
    val priorityQueue: PriorityQueue[Country] = PriorityQueue()(Ordering.by(sortByPlayers))
    priorityQueue ++= countries

    def cycle(): Boolean = {
      if (priorityQueue.isEmpty) {
        false
      } else {
        val list = ListBuffer.empty[Country]
        var count = 0

        while (priorityQueue.nonEmpty && count < k) {
          val c = priorityQueue.dequeue()

          if (c.players > 1) {
            list += Country(c.players - 1, c.name)
          }
          count += 1
        }
        priorityQueue ++= list

        count == k
      }
    }

    @tailrec def numCycles(count: Int): Int = {
      if (!cycle()) count
      else numCycles(count + 1)
    }

    numCycles(0)
  }

}
