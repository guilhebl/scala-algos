package code.array

/**
  * https://careercup.com/question?id=5156596605779968
  */
object InterleaveArrays {
  def main(args: Array[String]): Unit = {
    val list1 = List(1,2,3)
    val list2 = List(9,0)
    val list3 = List(5)
    val list4 = List(-4, -5, -2, -3, -1)

    println(getInterleaved(list1, list2, list3, list4))
  }

  def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
    case Nil    =>  Nil
    case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
  }

  def getInterleaved(lists : List[Int]*) : List[Int] = {
    transpose(lists.toList).flatten
  }
}
