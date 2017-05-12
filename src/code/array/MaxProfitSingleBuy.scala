package code.array

/**
 * https://careercup.com/question?id=5147092059160576
 */
object MaxProfitSingleBuy {
  
  def main(args: Array[String]): Unit = {   
    val a = Array(100, 120, 150, 180, 200, 40, 56, 90, 280)    
    println(getMaxProfitSingleBuy(a))
  }   
    
  def getMaxProfitSingleBuy(a:Array[Int]) : Int = {        
    var i = 0  
    var j = 0
    var max = -1
    var maxI = -1
    var maxJ = -1
    
    while (i < a.length - 1) {            
      while (i < a.length-1 && a(i) > a(i + 1)) i += 1
      
      j = i + 1
      while (j < a.length-1 && a(j) < a(j + 1)) j += 1
      
      // found local maxima
      if (a(j) - a(i) > max) {            
        max = a(j) - a(i)
        maxI = i
        maxJ = j
      }
      
      i += 1
    }
    return max
  }
  
}