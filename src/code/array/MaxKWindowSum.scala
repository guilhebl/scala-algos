package code.array

/**
 * https://careercup.com/question?id=5177437882155008
 */
object MaxWindowKSum {
  
  def main(args: Array[String]): Unit = {   
    val a = Array(10, 12, 15, 18, 20, 4, 5, 9, 28)    
    println(getMaxWindowKSum(a, 3))
  }   
    
  def getMaxWindowKSum(a:Array[Int], k:Int) : Int = {            
    var max = 0
    var i = 0

    while (i + k < a.length) {            
      val sum =  a.slice(i , i + k).sum
      if (max < sum) max = sum      
      i += 1
    }
    return max
  }
  
}