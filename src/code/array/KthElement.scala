package code.array

object KthElement {
  def main(args: Array[String]): Unit = {   
    val s = "aaa bbb ccc aaa bbb aaa"    
    println(getKthElement(s, 2))
  }   
    
  def getKthElement(s:String, k:Int) : String = {            
    s.split(" ").toList.foldLeft(Map.empty[String, Int]) { 
      (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) 
    }.toList.sortBy(- _._2).take(k).last._1
  }  
}