package code.graph

/**
 * https://careercup.com/question?id=5688021379710976
 */
object FindFamous {
  def main(args: Array[String]): Unit = {   
    val a = Array(
        ("Joe", "John", true), 
        ("Suzy", "John", true),
        ("Jay", "John", true),        
        ("Jack", "John", true),
        ("Jay", "Jack", false),
        ("Joe", "Suzy", true),        
        ("Jay", "Suzy", false),
        ("Jack", "Suzy", true),
        ("John", "Suzy", false)
    )     
    println(getFamous(a))
  }   
    
  def getFamous(arr:Array[(String,String,Boolean)]) : String = {            
    val a = arr.filter(_._3 == true)
    val copy = a.clone()
    
    val result = a.filter {
      case (p1,p2,r) => !copy.exists(_._1.equals(p2)) 
    }
    
    if (result.size > 0) {
      return result(0)._2
    }
    return ""
  }

}