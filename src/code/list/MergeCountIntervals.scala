package code.list

/**
https://careercup.com/question?id=5687609083297792
  */
object MergeCountIntervals {

  def main(args: Array[String]): Unit = {
    println(mergeSegments(Array(Array(1,4), Array(2,3))))
    println(mergeSegments(Array(Array(4,6), Array(1,2))))
    println(mergeSegments(Array(Array(1,4), Array(6,8), Array(2,4), Array(7,9), Array(10,15))))
  }

  def mergeSegments(segments: Array[Array[Int]]): Int = {
    if (segments == null || segments.length == 0) return 0

    val list = List.empty[Array[Int]]
    val merged = segments.sortWith(_(0) < _(0)).foldLeft(list)((seq, s) =>
      if (seq.isEmpty || seq.last(1) < s(0)) {
        seq :+ s
      } else {
        val last = seq.last
        seq.dropRight(1) :+ Array(last(0), Math.max(last(1), s(1)))
      }
    )

    merged.foldLeft(0)((acc, x) => acc + x(1) - x(0))
  }
}
