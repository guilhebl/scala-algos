package code.combinatorics

import scala.collection.mutable

object CoinSumCombination {
  def main(args: Array[String]): Unit = {
    printCoinCombinations(1000,10, 15, 55)
  }

  def printCoinCombinations(sum: Int, c1: Int, c2: Int, c3: Int) = {
    val sums = mutable.HashSet.empty[Int]
    sums.add(0)

    for (i <- 1 to sum) {
      if (sums.contains(i - c1) || sums.contains(i - c2) || sums.contains(i - c3)) {
        println(i)
        sums.add(i)
      }
    }
  }
}
