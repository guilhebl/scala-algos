package code.combinatorics

import scala.collection.mutable.ArrayBuffer

/**
 * https://careercup.com/question?id=5747122881167360
 */
object StringToCharMapping {
  
  val mappings = Map("AB" -> 'C', "BA" -> 'C', "AC" -> 'B', "CA" -> 'B', "BC" -> 'A', "CB" -> 'A')
  
  def main(args: Array[String]): Unit = {            
    println(toSingleCharNumChanges("ABACB"))
    println(toSingleCharNumChanges("ABACABB"))
  }   
    
  def toSingleCharNumChanges(s:String) : Int = {    
    var str = new StringBuilder(s)
    
    def f() : Int = {
      if (str.size == 0) return -1
      if (str.size == 1) return 0      
      if (str.sliding(2).map(a => a(0) == a(1)).count(identity) > 0) return -1      
      
      for (i <- 0 to str.size - 2) {
        val c = str(i)
        val n = str(i+1)
        if (c != n) {
          
          // backtracking
          str(i) = mappings(c+""+n)
          str.deleteCharAt(i+1)
          
          val steps = f()
          
          if (steps >= 0) return steps + 1
          
          // place chars back to original spots
          str(i) = c
          str(i + 1) = n
        }
      }
      return -1
    }        
    f()
  }
}