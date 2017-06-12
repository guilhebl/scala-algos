package code.string

/**
 * https://careercup.com/question?id=5675368674492416
 */
object NumKCount {
  
  def main(args: Array[String]): Unit = {        
    println(countK(30,3))
  }

  def countKDigit(n:Int, k:Int):Int = {
      var num = n
      var count = 0
      while (num > 10) {
        val digit = num % 10
        if (digit == k) {count += 1}
        num = num / 10
      }
      if (num == k) {count += 1}                       
      count
  }
  
  def countK(n:Int, k:Int):Int = {          
    1.to(n).foldLeft(0)((acc, x) => acc + countKDigit(x, k))
  }
}