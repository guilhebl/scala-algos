package code.array

import scala.collection.mutable.ListBuffer

/**
 * https://careercup.com/question?id=5693100010766336
 */
object MinKWindowSum {
  def main(args: Array[String]): Unit = {   
    
    val document =
      """This Hello World is a huge text with thousands
Java of Hello words and Scala other lines and World and many other Hello docs
Words of World in many langs Hello and features
Java Scala AXVX TXZX ASDQWE OWEQ World asb eere qwerer
asdasd Scala Java Hello docs World KLKM NWQEW ZXCASD OPOOIK Scala ASDSA
"""
    println(getMinWindowSize(document, "Hello World Scala"))
    println(getMinWindowSize2(document, "Hello World Scala".split(" ").toSet))
    println(getMinWindowSize3(document, "Hello World Scala"))
  }   
    
  def getMinWindowSize(str:String, s:String): Int = {

    /* creates a list of tuples List[(String, Int)] which contains each keyword and its
    respective index found in the text sorted in order by index.
    */
    val keywords = s.split(" ").toSet
    val idxs = keywords.map(k => (k -> ("(?i)\\Q" + k + "\\E").r.findAllMatchIn(str).map(_.start)))
      .map{ case (keyword,itr) => itr.map((keyword, _))}
      .flatMap(identity).toSeq
      .sortBy(_._2)

    // Calculates the min window on the next step.
    var min = Int.MaxValue
    var minI, minJ = -1

    // current window indexes and words
    var currIdxs = ListBuffer[Int]()
    var currWords = ListBuffer[String]()

    for(idx <- idxs ) {

      // check if word exists in window already
      val idxOfWord = currWords.indexOf(idx._1)

      if (!currWords.isEmpty && idxOfWord != -1) {
        currWords = currWords.drop(idxOfWord + 1)
        currIdxs = currIdxs.drop(idxOfWord + 1)
      }
      currWords += idx._1
      currIdxs += idx._2

      // if all keys are present check if it is new min window
      if (keywords.size == currWords.length) {
        val currMin = Math.abs(currIdxs.last - currIdxs.head)
        if (min > currMin) {
          min = currMin
          minI = currIdxs.head
          minJ = currIdxs.last
        }
      }
    }
    min
  }

  def getMinWindowSize2(document:String, WORDS:Set[String]): Int = {
    val minDistance = document.trim
      .split(" ")
      .foldLeft(List[(String, Int)](), None: Option[Int], 0) {
        case ((words, min, idx), word) if WORDS.contains(word) =>
          val newWords = (word, idx) :: words.filter(_._1 != word)
          if (newWords.map(_._1).toSet == WORDS) { // toSet on only 3 elmts
            var idxes = newWords.map(_._2)
            var dist = idxes.max - idxes.min
            var newMin = min match {
              case None => dist
              case Some(min) if min < dist => min
              case _ => dist
            }
            (newWords, Some(newMin), idx + word.length + 1)
          }
          else {
            (newWords, min, idx + word.length + 1)
          }
        case ((words, min, idx), word) =>
          (words, min, idx + word.length + 1)
      }
      ._2
    minDistance.getOrElse(-1)
  }

  def getMinWindowSize3(str :String, s :String) :Int = {
    val keywords = s.split("\\s+").toSet
    val re = "(?i)\\b(" + keywords.mkString("|") + ")\\b"
    val idxs = re.r.findAllMatchIn(str).map(w => w.start -> w.toString).toList

    def dist(input :List[(Int, String)], keys :Set[String]) :Option[Int] = input match {
      case Nil => None
      case (idx, word) :: t =>
        if (keys(word) && keys.size == 1) Some(idx)
        else dist(t, keys diff Set(word))
    }

    idxs.tails.collect{
      case (idx, word)::rest => dist(rest, keywords diff Set(word)).map(_ - idx)
    }.flatten.reduceOption(_ min _).getOrElse(-1)
  }
}