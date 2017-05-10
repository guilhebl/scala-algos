package code.combinatorics

/**
 * https://careercup.com/question?id=5154795351441408
 */
object Main {  
  
  val mapping = Map(1 -> "ABC", 2 -> "DEF") 
  
  def main(args: Array[String]): Unit = {            
    val a = mapping.keySet.toArray        
    var c = new Array[Char](a.length)
    printCombos(a, 0, c)
  }   
    
  def printCombos(a:Array[Int], i:Int, c:Array[Char]) : Unit = {    
    if (i == a.length) {			
      c foreach print
      println
		} else if (i < a.length) {
		  var j = 0		  
			for (j <- 0 to mapping(a(i)).length() - 1) {
				c(i) = mapping(a(i)).charAt(j)
				printCombos(a, i + 1, c)
			}
		}
	}

}