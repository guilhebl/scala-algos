package code.string

/**
 * https://careercup.com/question?id=5693819250016256
 */
object DiamondOddNumber {
 
  def main(args: Array[String]): Unit = {           
    printDiamondPattern(7)
  }
  
  def printDiamondPattern(num:Int) = {    
    def f(n:Int) = {
        val whitespace = (num - n) / 2        
        1.to(whitespace).foreach(elem => print(' '))        
        
        1.to(n).foreach(elem =>  
          if (elem % 2 == 1) print('o')
          else print('*')
        )
        
        1.to(whitespace).foreach(elem => print(' '))        
        println
    }
    
    1.to(num).filter(n => n % 2 != 0).foreach(n => f(n))
    (num-1).to(1).by(-1).filter(n => n % 2 != 0).foreach(n => f(n))    
  }
  
}