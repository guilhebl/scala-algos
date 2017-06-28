package code.string

/**
 * https://careercup.com/question?id=5748104113422336
 */
object WordCount {
 
  def main(args: Array[String]): Unit = {           
    println(countWords("aaa bbb ccc aaa bbb aaa", 2))
  }
  
  def countWords(s:String, k:Int):String = {        
    s.split(" ").toList.foldLeft(Map.empty[String, Int]) { 
      (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) 
    }.toList.sortBy(- _._2).take(k).last._1    
  }
  
}