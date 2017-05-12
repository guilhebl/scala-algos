package code.sort

/**
 * https://careercup.com/question?id=5173759511101440
 */
object WordsBinarySearch {
  
  def main(args: Array[String]): Unit = {   
    val a = Array("pto", "ret", "supp", "und", "xen", "asyn", "bah", "bano", "ccc", "ddd")
    println(getIndexRotationPoint(a))
  }   
    
  def getIndexRotationPoint(a:Array[String]) : Int = {
    var i = 0
    var j = a.length-1
    
    while (i < j) {
      val m = a.length / 2
      
      if (m >= 1 && a(m).compareTo(a(m-1)) < 0) {
        return m
      } else if (m < a.length+1 && a(m).compareTo(a(m+1)) > 0) {
        return m+1
      }
      
      if (a(i).compareTo(a(m)) < 0) {
        i = m + 1
      } else {
        j = m - 1
      }       
    }
    
    return i
  }

}