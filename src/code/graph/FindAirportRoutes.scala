package code.graph

import scala.annotation.tailrec

/**
  *
  * Unordered: ("ITO", "KOA"), ("ANC", "SEA"), ("LGA", "CDG"), ("KOA", "LGA"), ("PDX", "ITO"), ("SEA", "PDX")
  * Ordered: ("ANC", "SEA"), ("SEA", "PDX"), ("PDX", "ITO"), ("ITO", "KOA"), ("KOA", "LGA"), ("LGA", "CDG")
  *
  * https://careercup.com/question?id=5713295334965248
  */
object FindAirportRoutes {

    def main(args: Array[String]): Unit = {
      val a = Seq(("ITO", "KOA"), ("ANC", "SEA"), ("LGA", "CDG"), ("KOA", "LGA"), ("PDX", "ITO"), ("SEA", "PDX"))
      printOrderedRoute(a)
      println("Using topological sort: " + tsort(a))
    }

    def printOrderedRoute(a : Seq[(String,String)]) : Unit = {
      val sources = a.groupBy(_._1).map { case (k,v) => (k,v.length)}
      val destinations = a.groupBy(_._2).map { case (k,v) => (k,v.length)}
      val origin = sources.filter(p => destinations.getOrElse(p._1, 0) < p._2)

      def dfs(source :String, tickets: Seq[(String,String)], path: Seq[(String,String)]): Unit = {
        if (tickets.isEmpty) {
          println(path)
        } else {
          tickets.filter(_._1.equals(source)).foreach { t =>
            dfs(t._2, tickets.filterNot(x => t._1 == x._1 && t._2 == x._2), path :+ t)
          }
        }
      }
      // try all possible origin tickets
      origin.foreach(or => dfs(or._1, a, Seq.empty[(String, String)]))
    }

  /**
    * Topological sort using generic types
    *
    * @param edges
    * @tparam A
    * @return
    */
  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.map { _._1 }
        tsort(hasPreds.mapValues { _ -- found }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
}
