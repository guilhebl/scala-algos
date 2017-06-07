package code.string

/**
 * https://careercup.com/question?id=5712666707361792
 */
object CheckNumber {
 
  def main(args: Array[String]): Unit = {           
    println(checkNum(25,2))
  }
  
  def checkNum(x:Int, y:Int):Int = {    
    var count = 0
    
    def countFreq(n:Int) = {      
      var num = n
      while (num > 0) {
        if (num % 10 == y) count += 1                
        num = num / 10
      }      
    }
    
    1.to(x).foreach(n => countFreq(n))
    count    
  }
  
}