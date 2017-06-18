package code.array

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

/**
 * https://careercup.com/question?id=5693100010766336
 */
object MinKWindowSum {
  def main(args: Array[String]): Unit = {   
    
    val document = "MS(1) Awesome x y Is MS(5) x y MS(8) x Is Awesome x y z Awesome"    
    println(getMinWindowSize(document, "MS is awesome"))
  }   
    
  def getMinWindowSize(doc:String, s:String): Int = {
    
    val keywords = s.split(" ").toSet
    val idxs = keywords.map(k => (k -> ("(?i)\\Q" + k + "\\E").r.findAllMatchIn(doc).map(_.start)))
    .map{ case (keyword,itr) => itr.foldLeft(List[(String,Int)]())((result, num) => result :+ (keyword, num))}
    .foldLeft(List[(String,Int)]())((res, list) => res ++ list)
    .sortBy(_._2)
        
    var min = Int.MaxValue    
    var minI = 0
    var minJ = 0
    var currWindow = ListBuffer[(String,Int)]()
    
    for( tuple <- idxs ) {  
      if (!currWindow.isEmpty && currWindow.head._1.equals(tuple._1)) currWindow.remove(0)         
      currWindow += tuple
      if (keywords.subsetOf(currWindow.map(_._1).toSet)) {
        val currMin = currWindow.last._2 - currWindow.head._2
        if (min > currMin) {
          min = currMin
          minI = currWindow.head._2
          minJ = currWindow.last._2          
        }
      }      
    }
        
    println("min = " + min + " ,i = " + minI + " j = " + minJ )    
    min
  }
 
}