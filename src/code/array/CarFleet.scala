package code.array

import scala.annotation.tailrec

case class Car(position: Int, time: Double)

object CarFleet {

  def main(args: Array[String]): Unit = {
    println(carFleet(
      10,
      Array(6, 8),
      Array(3, 2)
    ))
  }

  def carFleet(target: Int, position: Array[Int], speed: Array[Int]): Int = {
    val cars = position.view.zipWithIndex.foldLeft(Array.empty[Car])((arr, pos) =>
      arr :+ Car(pos._1, (target - pos._1).toDouble / speed(pos._2))
    ).sortBy(_.position)

    @tailrec def f(cars: Seq[Car], i: Int, count: Int): (Int, Int) = {
      if (i <= 0) (i, count)
      else if (cars(i).time < cars(i - 1).time)
        f(cars, i - 1, count + 1)
      else f(cars.updated(i - 1, cars(i)), i - 1, count)
    }

    val count = f(cars, position.length - 1, 0)
    val diff = if (count._1 == 0) 1 else 0
    count._2 + diff
  }

}

