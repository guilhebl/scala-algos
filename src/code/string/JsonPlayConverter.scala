package code.string

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.PrintWriter

object JsonPlayConverter {
 
  def main(args: Array[String]): Unit = {   
    try {
      convert(); 
    } catch {
      case ex: Exception => println(ex)
    }
  }
  
  def convert() = {    
    
    val filename = "src/code/string/input.txt"
    var sb = new StringBuilder()
    
    var className = ""
    var fieldList:ListBuffer[(String, String)] = ListBuffer[(String, String)]()
    
    for (line <- Source.fromFile(filename).getLines()) {      
      val tokens = line.split(" ")
      val prefix = tokens(0)
      
      prefix match {
        case "case" => className = tokens(2)        
        case "package" => println("ignore package")
        case "import" => println("ignore import")
        case "//" => println("ignore //")
        case "/**" => println("ignore /**")        
        case ")" => println("ignore )")
        case _ => {
          
          val filtered = line.replaceAll(" ", "")
          if (!filtered.trim().equals("")) {
            
            val fieldTokens = filtered.split(":") 
            val fieldName = fieldTokens(0)
            val fieldVal = fieldTokens(1)
            
            println("inserting fields " + fieldName + " : " + fieldVal)
                        
            if (fieldVal.endsWith(",")) {
              fieldList += new Tuple2(fieldName, fieldVal.dropRight(1))
            } else {
              fieldList += new Tuple2(fieldName, fieldVal)
            }            
          }
        }
      }           
    }
    
    println("class name = " + className)
    
    var fieldsStr = new StringBuilder()
    for (e <- fieldList) {
      fieldsStr ++= "(__ \\ \"" + e._1 + "\").format[" + e._2 + "] and" + "\n"
    }
    
    // remove last and
    fieldsStr = fieldsStr.dropRight(4)
    
    println("fieldsStr = " + fieldsStr)
    
    // output
    val str = s"""
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    object $className {
      implicit val documentFormatter: Format[$className] = (
       $fieldsStr
      )($className.apply, unlift($className.unapply))
    }"""
          
    // write to file
    new PrintWriter("src/code/string/output.txt") { 
      write(str); close 
    }    
  }
  
}