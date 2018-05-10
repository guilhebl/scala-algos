package code.array

/**
  * https://careercup.com/question?id=4855176404926464
  */
object SumSubsequence {
  def main(args: Array[String]): Unit = {
    val subArray = Array(1,2,3)
    val interval = (3,6)
    println(getCountSumSubarray(subArray, interval))
  }

  def getCountSumSubarray(subArray : Array[Int], interval : (Int,Int)) : Int = {

    var count = 0 // to count num of subsequenecs within range
    var sum = 0
    val n = subArray.length

    for (i <- 0 until n) {
      for (j <- i until n) {
        for (k <- i to j) {
          sum += subArray(k)
          print(subArray(k))
        }

        if (sum >= interval._1 && sum <= interval._2) {
          count += 1
        }
        sum = 0
      }
    }
    count
  }
}