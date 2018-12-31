package code.array

/**
 * http://www.geeksforgeeks.org/reverse-an-array-without-affecting-special-characters/
 */
object ReverseArraySpecialChars {
  
  def main(args: Array[String]): Unit = {
    val a = Array('a','!','!','!','b','.','c','.','d',',','e',''','f',',','g','h','i')
    reverseArraySpecialChars(a) foreach print
  }   
    
  def reverseArraySpecialChars(a:Array[Char]) : Array[Char] = {
    var i = 0
    var j = a.length-1    
    val isSpecialChar = (x:Char) => x == '?' || x == ',' || x =='!' || x == '.' || x == '''
        
    while(i < j) {
      if (isSpecialChar(a(i))) i += 1
      else if (isSpecialChar(a(j))) j -= 1
      else {
        val temp = a(i)
        a(i) = a(j)
        a(j) = temp
        i += 1
        j -= 1        
      }
    }
    
    a
  }

  
}