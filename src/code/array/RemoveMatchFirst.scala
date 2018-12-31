package code.array

/**
  * https://careercup.com/question?id=5750713720766464
  */
object RemoveMatchFirst {
  def main(args: Array[String]): Unit = {
    println(removeMaxCool(Vector(1,2,3,100,12,23,44)))

    val listOfNames = List("john", "melanie", "maya", "jack")

    val fullName = "john legend"
    val exists = listOfNames.exists(fullName.startsWith)

    println(exists)
  }
  def removeMaxCool(xs: Vector[Int]) = xs.filter(_ < xs.max)
}

